package me.katze.gui4s.example

import api.impl.{DrawMonad, DrawMonadT, HighLevelApiImpl, LayoutApiImpl, LayoutPlacement, LayoutPlacementMeta}
import api.{AdditionalAxisPlacementStrategy, HighLevelApi, LabelApi, LayoutApi, MainAxisPlacementStrategy}
import draw.{DrawApi, NotifyDrawLoopWindow, ProcessRequestImpl, SimpleDrawApi, windowBounds}
import place.{RunPlacement, additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}
import task.{IOOnThread, MultiMap, RefTaskSet, StlWrapperMultiMap, WidgetTaskImpl, runWidgetTask}
import update.ApplicationRequest

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.stateful.{EventReaction, Mergeable, Path, TaskFinished}
import update.ProcessRequest

import me.katze.gui4s.example.given
import me.katze.gui4s.widget.library.given
import cats.effect.std.{AtomicCell, Queue}
import draw.swing.{SwingApi, SwingWindow}

import me.katze.gui4s.widget.{EventResult, RunnableIO, Widget, given}
import me.katze.gui4s.widget

import scala.math.Numeric.Implicits.*

type Draw[MU, T] = ReaderT[IO, (MU, MU), T]
type DrawT[MU] = [T] =>> Draw[MU, T]

given[MU](using drawMonad: DrawMonad[DrawT[MU], MU]): LayoutDraw[Draw[MU, Unit], LayoutPlacementMeta[MU]] with
  override def drawChildren(children: List[(Draw[MU, Unit], LayoutPlacementMeta[MU])]): Draw[MU, Unit] =
    children.traverse_((childDraw, coordinates) => drawMonad.move(coordinates.x, coordinates.y, childDraw))
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

object WindowResized 


trait Gui4sApp[MU : Fractional] extends IOApp:
  type DownEvent = TaskFinished | WindowResized.type
  type WidgetTaskT[F[+_]] = [T] =>> WidgetTaskImpl[F, T]

  def rootWidget(using api: HighLevelApi & LayoutApi[MU] & LabelApi[Unit]) : api.Widget[ApplicationRequest]
  
  private type Place[+T] = Measurable[MU, T]
  type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
  type TextStyle = Unit
  
  given[Task] : LiftEventReaction[Update[Task], Task] with
    override def lift[A, B](reaction: EventReaction[Task, A, B]): Update[Task][A, B] =
      EventResult[Task, A, B](reaction.newState, reaction.parentEvent, reaction.ios.map(RunnableIO(_, Path(), _))) // TODO Проверить, что тут правда нужен пустой путь
    end lift
  end given
  
  private def runDraw(draw: DrawT[MU][Unit]) : IO[Unit] =
    draw.run(Numeric[MU].zero, Numeric[MU].zero)
  end runDraw
  
  final override def run(args: List[String]): IO[ExitCode] =
    for
      queue <- Queue.unbounded[IO, DownEvent]
      (swing, unswing) <- SwingApi.invoke[MU]((a, b) => NotifyDrawLoopWindow(SwingWindow(a, b), queue.offer(WindowResized))).allocated
      code <- run2[IO, DrawT[MU], Place, [A] =>> WidgetTaskImpl[IO, A]](
        queue = queue,
        api = swing, 
        runDraw = runDraw, 
        startTask = [T] => (task : WidgetTaskImpl[IO, T], drain : T => IO[Unit]) => startWidgetTask(task, drain),
        drawLoopExceptionHandler = drawLoopExceptionHandler
      )(
        using
          summon,
          summon,
          new HighLevelApiImpl[
            Update[WidgetTaskImpl[IO, Any]],
            DrawT[MU][Unit],
            Place,
            RecompositionEffect[IO, WidgetTaskImpl[IO, Any]],
            WidgetTaskT[IO],
            MU,
            TextStyle,
            DownEvent
          ](swing.graphics, containerPlacementCurried)
              .asInstanceOf[HL[[T] =>> Place[widget.Widget[Update[WidgetTaskImpl[IO, Unit]], Draw[MU, Unit], Place, RecompositionEffect[IO, WidgetTaskImpl[IO, Any]], T, DownEvent]], [T] =>> WidgetTaskImpl[IO, T], MU]],
          summon,
      )
    yield code
  end run

  private def startWidgetTask[F[+_] : Concurrent, T](task: WidgetTaskImpl[F, T], resultDrain: T => F[Unit]): F[Fiber[F, Throwable, Unit]] =
    // TODO add execution context
    Concurrent[F].start(runWidgetTask(task, resultDrain))
  end startWidgetTask

  private def run2[
    F[+_] : Concurrent,
    Draw[_] : DrawMonadT[MU],
    Placement[+_],
    WidgetTask[+_]
  ](
    queue : Queue[F, DownEvent],
    api: DrawApi[F, MU],
    runDraw : Draw[Unit] => F[Unit],
    startTask : [T] => (WidgetTask[T], T => F[Unit]) => F[Fiber[F, Throwable, Unit]],
    drawLoopExceptionHandler: DrawLoopExceptionHandler[F, Throwable],
  )(
      using
        HL[[T] =>> Placement[widget.Widget[Update[WidgetTask[Unit]], Draw[Unit], Placement, RecompositionEffect[F, WidgetTask[Any]], T, DownEvent]], WidgetTask, MU],
        Lift[F, Draw, (MU, MU)],
  ) : F[ExitCode] =
    for
      taskSet <- Ref.of[F, MultiMap[Path, IOOnThread[F]]](StlWrapperMultiMap(Map()))
      given RunPlacement[F, Placement] = MeasurableRunPlacement(windowBounds(api.window)).asInstanceOf[RunPlacement[F, Placement]] // TODO
      given ProcessRequest[F, ApplicationRequest] = ProcessRequestImpl(api.window)

      rootWidget <- rootWidget.runPlacement

      widget <- AtomicCell[F].of(
        EventConsumerAdapter(
          rootWidget,
          RefTaskSet[F, WidgetTask[Any]](taskSet, (path, task) => startTask(task, offerTask(queue, path, _))),
        )
      )
      graphics = api.graphics[Draw]
      code <- applicationLoop[F, ApplicationRequest, DownEvent, [A, B] =>> EventConsumerAdapter[F, Draw[Unit], Placement, WidgetTask[Any], A, B]](
        eventBus = queue,
        widgetCell = widget,
        drawLoop = currentWidget => drawLoop(drawLoopExceptionHandler, runDraw(graphics.beginDraw), runDraw(graphics.endDraw))(currentWidget.map(_.draw).flatMap(runDraw)),
        updateLoop = updateLoop
      ).flatMap(_.join)
    yield code
  end run2

  def offerTask[F[+_]](queue: Queue[F, ? >: TaskFinished], at: Path, taskResult: Any) : F[Unit] =
    queue.offer(TaskFinished(at, taskResult))
  end offerTask

  given LabelPlacement[Place[LayoutPlacementMeta[MU]], TextStyle] with
    override def sizeText(text: String, options: TextStyle): Place[LayoutPlacementMeta[MU]] =
      _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(17))
    end sizeText
  end given
  
  private def containerPlacementCurried: LayoutPlacement[Update[WidgetTaskImpl[IO, Any]], DrawT[MU][Unit], Place, RecompositionEffect[IO, WidgetTaskImpl[IO, Any]], DownEvent, MU] =
    [Event] => (axis : Axis, elements, main, additional) =>
      weightedRowColumnPlace[MU, widget.Widget[Update[WidgetTaskImpl[IO, Any]], DrawT[MU][Unit], Place, RecompositionEffect[IO, WidgetTaskImpl[IO, Any]], Event, DownEvent]](
        axis,
        elements.map(widget => MaybeWeighted(None, widget)),
        rowColumnPlace(_, _, mainAxisStrategyPlacement[MU](main, _, _), additionalAxisStrategyPlacement[MU](additional, _, _))
      ).map(unpack)
  end containerPlacementCurried

  private def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp
