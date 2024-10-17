package me.katze.gui4s.example

import api.impl.{DrawMonad, DrawMonadT, HighLevelApiImpl, LayoutApiImpl, LayoutPlacementMeta}
import api.{AdditionalAxisPlacementStrategy, HighLevelApi, LabelApi, LayoutApi, MainAxisPlacementStrategy}
import draw.{DrawApi, ProcessRequestImpl, SimpleDrawApi, windowBounds}
import place.{RunPlacement, additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}
import task.{IOOnThread, MultiMap, RefTaskSet, StlWrapperMultiMap, WidgetTaskImpl, runWidgetTask}
import update.ApplicationRequest

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.lowlevel.{WidgetLibrary, WidgetLibraryImpl}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{EventReaction, Path, TaskFinished, given}
import update.ProcessRequest

import me.katze.gui4s.widget.library.given
import cats.effect.std.Queue
import me.katze.gui4s.example.draw.swing.SwingApi
import me.katze.gui4s.widget.{EventResult, RunnableIO, given}
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
  def rootWidget(using api: HighLevelApi & LayoutApi[MU] & LabelApi[Unit]) : api.Widget[ApplicationRequest]
  
  type Place[+T] = Measurable[MU, T]
  type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
  type TextStyle = Unit
  
  given[Task] : LiftEventReaction[Update[Task], Task] with
    override def lift[A, B](reaction: EventReaction[Task, A, B]): Update[Task][A, B] =
      EventResult[Task, A, B](reaction.newState, reaction.parentEvent, reaction.ios.map(RunnableIO(_, Path(), _))) // TODO Проверить, что тут правда нужен пустой путь
    end lift
  end given
  
  def runDraw(draw : DrawT[MU][Unit]) : IO[Unit] =
    draw.run(Numeric[MU].zero, Numeric[MU].zero)
  end runDraw
  
  final override def run(args: List[String]): IO[ExitCode] =
    for
      swing <- SwingApi.invoke
      lowLevelLib = WidgetLibraryImpl[Update[WidgetTaskImpl[IO, Any]], Draw[MU, Unit], Place, DownEvent]()
      code <- run2(using lowLevelLib)(
        api = swing, 
        runDraw = runDraw, 
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

  def startWidgetTask[F[+_] : Concurrent, T](task : WidgetTaskImpl[F, T], resultDrain: T => F[Unit]): F[Fiber[F, Throwable, Unit]] =
    // TODO add execution context
    Concurrent[F].start(runWidgetTask(task, resultDrain))
  end startWidgetTask

  def run2[
    F[+_] : Concurrent,
    Draw[_] : DrawMonadT[MU],
    Placement[+_],
    WidgetTask[+_]
  ](
    using wl: WidgetLibraryImpl[Update[WidgetTask[Any]], Draw[Unit], Placement, DownEvent]
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

  given LabelPlacement[Place[LayoutPlacementMeta[MU]], TextStyle] with
    override def sizeText(text: String, options: TextStyle): Place[LayoutPlacementMeta[MU]] =
      _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(10))
    end sizeText
  end given
  
  private def higherApi(lowLevelApi: WidgetLibraryImpl[Update[WidgetTaskImpl[IO, Any]], Draw[MU, Unit], Place, DownEvent], drawApi: SimpleDrawApi[MU, Draw[MU, Unit]]) : HL[lowLevelApi.Widget, WidgetTaskT[IO], MU] =
    given lowLevelApi.type = lowLevelApi

    new HighLevelApiImpl[
      Update[WidgetTaskImpl[IO, Any]], 
      IO,
      DrawT[MU], 
      Place,
      WidgetTaskT[IO],
      MU, 
      TextStyle,
      DownEvent
    ](drawApi) with LayoutApiImpl[MU](containerPlacementCurried) {
      override type Widget[+T] = wl.Widget[T]
      override type WidgetTask[+T] = WidgetTaskImpl[IO, T]
      override val wl: lowLevelApi.type = lowLevelApi
    }.asInstanceOf[HL[lowLevelApi.Widget, WidgetTaskT[IO], MU]] // TODO do not believe me
  end higherApi
  
  private def containerPlacementCurried
        (using wl: WidgetLibrary { type PlacementEffect[+T] = Place[T] }): [Event] => (Axis, List[wl.Widget[Event]], MainAxisPlacementStrategy[MU], AdditionalAxisPlacementStrategy) 
        => wl.PlacementEffect[List[(wl.PlacedWidget[Event, wl.SystemEvent], LayoutPlacementMeta[MU])]] =
    [Event] => (axis, elements, main, additional) => containerPlacement(axis, elements, main, additional)
  end containerPlacementCurried
  
  private def containerPlacement[Event](
    using wl: WidgetLibrary { type PlacementEffect[+T] = Place[T] }
  )(
    mainAxis : Axis,
    elements : List[wl.Widget[Event]],
    main : MainAxisPlacementStrategy[MU],
    additional : AdditionalAxisPlacementStrategy
  ) : wl.PlacementEffect[List[(wl.PlacedWidget[Event, wl.SystemEvent], LayoutPlacementMeta[MU])]] =
    weightedRowColumnPlace[MU, wl.PlacedWidget[Event, wl.SystemEvent]](
      mainAxis,
      elements.map(widget => MaybeWeighted(None, widget)),
      rowColumnPlace(_, _, mainAxisStrategyPlacement[MU](main, _, _), additionalAxisStrategyPlacement[MU](additional, _, _))
    ).map(unpack)
  end containerPlacement

  def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp
