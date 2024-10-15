package me.katze.gui4s.example

import api.impl.{DrawMonad, DrawMonadT, HighLevelApiImpl, LayoutApiImpl, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.{DrawApi, ProcessRequestImpl, SimpleDrawApi, windowBounds}
import place.{RunPlacement, additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}
import task.{IOOnThread, MultiMap, RefTaskSet, StlWrapperMultiMap, WidgetTaskImpl}
import update.ApplicationRequest

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.lowlevel.WidgetLibraryImpl
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{EventReaction, Path, TaskFinished, given}
import update.ProcessRequest

import me.katze.gui4s.widget.library.{LayoutLibrary, given}
import cats.effect.std.Queue
import me.katze.gui4s.example.draw.swing.SwingApi
import me.katze.gui4s.widget.{EventResult, given}

import me.katze.gui4s.widget.stateful.{BiMonad, given}
import scala.math.Numeric.Implicits.*

type Draw[MU, T] = ReaderT[IO, (MU, MU), T]
type DrawT[MU] = [T] =>> Draw[MU, T]

given[MU](using drawMonad: DrawMonad[DrawT[MU], MU]): LayoutDraw[Draw[MU, Unit], LayoutPlacementMeta[MU]] with
  override def drawChildren(children: List[(Draw[MU, Unit], LayoutPlacementMeta[MU])]): Draw[MU, Unit] =
    children.traverse_((childDraw, coords) => drawMonad.move(coords.x, coords.y, childDraw))
  end drawChildren
end given

given [MU: Numeric] : DrawMonad[DrawT[MU], MU] with
  override def move[T](dx: MU, dy: MU, effect: Draw[MU, T]): Draw[MU, T] =
    ReaderT.apply((x, y) => effect.run(x + dx, y + dy))
  end move
end given

type HL[W[+_], WT[+_], MU] <: HighLevelApi & LabelApi[Unit] & LayoutApi[MU]
  {
    type Widget[+T] = W[T]
    type WidgetTask[+T] = WT[T]
  }

type DownEvent = TaskFinished
type WidgetTaskT[F[+_]] = [T] =>> WidgetTaskImpl[F, T]

trait Gui4sApp[MU : Fractional] extends IOApp:
  type Update[Task] = [A, B] =>> EventResult[Task, A, B]
  type Merge[Task] = [A] =>> Update[Task][A, Nothing]
  
  given runMerge[Task] : ([T] => Merge[Task][T] => Update[Task][T, Nothing]) = [T] => (a : Merge[Task][T]) => a
  // TODO Оказалось, что свапать эти эффекты просто так нельзя. Надо думать, что с этим делать.
  given swapEffects[Task] : ([T] => MeasurableT[MU][Merge[Task][T]] => Merge[Task][MeasurableT[MU][T]]) =
    ???//[T] => measurable => EventResult[MeasurableT[MU][T], T, Nothing](bounds => measurable(bounds).widget, ???, ???)
  end swapEffects
  
  given[Task] : LiftEventReaction[Update[Task], Task] with
    override def lift[A, B](reaction: EventReaction[Task, A, B]): Update[Task][A, B] = ???
  end given
  
  final override def run(args: List[String]): IO[ExitCode] =
    for
      swing <- SwingApi.invoke
      lowLevelLib = WidgetLibraryImpl[Update[WidgetTaskImpl[IO, Any]], Merge[WidgetTaskImpl[IO, Any]], Draw[MU, Unit], MeasurableT[MU], DownEvent](swapEffects[WidgetTaskImpl[IO, Any]])
      code <- run2[IO, DrawT[MU], MeasurableT[MU], WidgetTaskT[IO]](using lowLevelLib)(
        api = swing, 
        runDraw = _.run(Numeric[MU].zero, Numeric[MU].zero), 
        startTask = [T] => (task, drain) => startWidgetTask(task, drain),
        drawLoopExceptionHandler = drawLoopExceptionHandler
      )(
        using
          summon,
          summon,
          higherApi(lowLevelLib, swing.graphics),
          MeasurableRunPlacement(windowBounds(swing.window)),
          summon,
          ProcessRequestImpl(swing.window)
      )
    yield code
  end run

  def run2[
    F[+_] : Concurrent,
    Draw[_] : DrawMonadT[MU],
    Placement[+_],
    WidgetTask[+_]
  ](
    using wl: WidgetLibraryImpl[[A, B] =>> Update[WidgetTask[Any]][A, B], Merge[WidgetTask[Any]], Draw[Unit], Placement, DownEvent]
  )(
    api: DrawApi[F, MU],
    runDraw : Draw[Unit] => F[Unit],
    startTask : [T] => (WidgetTask[T], T => F[Unit]) => F[Fiber[F, Throwable, Unit]],
    drawLoopExceptionHandler: DrawLoopExceptionHandler[F, Throwable]
  )(
    using
      HL[wl.Widget, WidgetTask, MU], 
      RunPlacement[F, Placement], 
      Lift[F, Draw, (MU, MU)], 
      ProcessRequest[F, ApplicationRequest]
  ) : F[ExitCode] =
    runWidget(using wl)(
      widget = queue =>
        for
          taskSet <- Ref.of[F, MultiMap[Path, IOOnThread[F]]](StlWrapperMultiMap(Map()))
          widget <- rootWidget.runPlacement
        yield EventConsumerAdapter(using wl)(
          widget,
          RefTaskSet[F, WidgetTask[Any]](taskSet, (path, task) => startTask(task, offerTask(queue, path, _))),
        ),
      drawLoopExceptionHandler = drawLoopExceptionHandler,
      api = api.graphics,
      runDraw = runDraw
    )
  end run2


  def offerTask[F[+_]](queue: Queue[F, TaskFinished], at: Path, taskResult: Any) : F[Unit] =
    queue.offer(TaskFinished(at, taskResult))
  end offerTask

  def startWidgetTask[F[+_] : Concurrent, T](task: WidgetTaskImpl[F, T], resultDrain: T => F[Unit]) : F[Fiber[F, Throwable, Unit]] =
    // TODO add execution context
    Concurrent[F].start(runWidgetTask(task, resultDrain))
  end startWidgetTask

  def runWidgetTask[F[+_] : Concurrent, T](widgetTask : WidgetTaskImpl[F, T], offerTask : T => F[Unit]) : F[Unit] =
    widgetTask match
      case WidgetTaskImpl.OneEvent(value) => value.flatMap(offerTask)
      case WidgetTaskImpl.ManyEvents(stream) => stream.evalMap(offerTask).compile.drain
    end match
  end runWidgetTask


  type TextStyle = Unit

  private def higherApi(lowLevelApi: WidgetLibraryImpl[Update[WidgetTaskImpl[IO, Any]], Merge[WidgetTaskImpl[IO, Any]], Draw[MU, Unit], MeasurableT[MU], DownEvent], drawApi: SimpleDrawApi[MU, Draw[MU, Unit]]) : HL[lowLevelApi.Widget, WidgetTaskT[IO], MU] =
    given LabelPlacement[Measurable[MU, LayoutPlacementMeta[MU]], TextStyle] with
      override def sizeText(text : String, options: TextStyle): Measurable[MU, LayoutPlacementMeta[MU]] =
        _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(10))
    given lowLevelApi.type = lowLevelApi
    given runMerge2 : ([T] => (Merge[WidgetTaskImpl[IO, Any]][T]) => Update[WidgetTaskImpl[IO, Any]][T, Nothing]) = runMerge[WidgetTaskImpl[IO, Any]]
    given swapEffects2 : ([T] => MeasurableT[MU][Merge[WidgetTaskImpl[IO, Any]][T]] => Merge[WidgetTaskImpl[IO, Any]][MeasurableT[MU][T]]) = swapEffects[WidgetTaskImpl[IO, Any]]
    given layoutLib : LayoutLibrary[lowLevelApi.type, LayoutPlacementMeta[MU]] = layoutLibraryImpl[lowLevelApi.type, Update[WidgetTaskImpl[IO, Any]], Merge[WidgetTaskImpl[IO, Any]], Draw[MU, Unit], lowLevelApi.PlacedWidget, lowLevelApi.PlacementEffect, LayoutPlacementMeta[MU], DownEvent]
    new HighLevelApiImpl[Update[WidgetTaskImpl[IO, Any]], Merge[WidgetTaskImpl[IO, Any]], IO, DrawT[MU], MeasurableT[MU], WidgetTaskT[IO], MU, TextStyle, DownEvent](using lowLevelApi)(drawApi) 
        with LayoutApiImpl[MU](lowLevelApi)(
      [Event] => (axis, elements, main, additional) => weightedRowColumnPlace[MU, lowLevelApi.PlacedWidget[Event, lowLevelApi.SystemEvent]](
        axis,
        elements.map(widget => MaybeWeighted(None, widget)),
        rowColumnPlace(_, _, mainAxisStrategyPlacement[MU](main, _, _), additionalAxisStrategyPlacement[MU](additional, _, _))
      ).map(unpack)
    ) {
      override type Widget[+T] = wl.Widget[T]
      override type WidgetTask[+T] = WidgetTaskImpl[IO, T]
      
      override val wl: lowLevelApi.type = lowLevelApi
    }.asInstanceOf[HL[lowLevelApi.Widget, WidgetTaskT[IO], MU]] // TODO do not believe me
  end higherApi

  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler

  def rootWidget(using api: HighLevelApi & LayoutApi[MU] & LabelApi[Unit]) : api.Widget[ApplicationRequest]
end Gui4sApp
