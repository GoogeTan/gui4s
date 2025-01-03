package me.katze.gui4s.example

import api.impl.{DrawMonad, HighLevelApiImpl, LayoutPlacement, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.{NotifyDrawLoopWindow, ProcessRequestImpl, windowBounds}
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
import me.katze.gui4s.widget.stateful.{EventReaction, Path, TaskFinished}
import update.ProcessRequest

import me.katze.gui4s.example.given
import me.katze.gui4s.widget.library.given
import cats.effect.std.{AtomicCell, Queue}
import draw.swing.{SwingApi, SwingWindow}

import me.katze.gui4s.layout.given
import me.katze.gui4s.widget.{EventResult, RunnableIO, Widget, given}
import me.katze.gui4s.widget.stateful.{*, given}
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

object WindowResized 


trait Gui4sApp[MU : Fractional] extends IOApp:
  type DownEvent = TaskFinished | WindowResized.type
  type WidgetTaskT[F[+_]] = [T] =>> WidgetTaskImpl[F, T]

  def rootWidget[T <: HighLevelApi & LayoutApi[MU] & LabelApi[Unit]](using api: T) : api.Widget[ApplicationRequest]
  
  private type Place[+T] = Measurable[MU, T]
  type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
  type TextStyle = Unit
  
  given[Task] : LiftEventReaction[Update[Task], Task] with
    override def lift[A, B](reaction: EventReaction[Task, A, B]): Update[Task][A, B] =
      EventResult[Task, A, B](reaction.newState, reaction.parentEvent, reaction.ios.map(RunnableIO(_, Path(List("_ROOT_")), _))) // TODO Проверить, что тут правда нужен пустой путь
    end lift
  end given
  
  private def runDraw(draw: DrawT[MU][Unit]) : IO[Unit] =
    draw.run(Numeric[MU].zero, Numeric[MU].zero)
  end runDraw
  
  enum RecompositionAction:
    case Task(task : RunnableIO[WidgetTaskImpl[IO, Any]])
    case KillTasksFor(path : Path)
  end RecompositionAction

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final override def run(args: List[String]): IO[ExitCode] =
    type Recomposition = List[RecompositionAction]
    given KillTasks[Recomposition] = path => List(RecompositionAction.KillTasksFor(path))

    for
      queue <- Queue.unbounded[IO, DownEvent]
      (drawApi, destroyDrawApi) <- SwingApi.invoke[MU]((frame, windowComponent) => NotifyDrawLoopWindow(SwingWindow(frame, windowComponent), queue.offer(WindowResized))).allocated
      taskMap <- Ref.of[IO, MultiMap[Path, IOOnThread[IO]]](StlWrapperMultiMap(Map()))
      taskSet = RefTaskSet[IO, WidgetTaskImpl[IO, Any]](
        taskMap, 
        (path, task) => startWidgetTask(task, offerTask(queue, path, _))
      )

      widgetApi = new HighLevelApiImpl[
        Update[WidgetTaskImpl[IO, Any]],
        Draw[MU, Unit],
        Place,
        Recomposition,
        WidgetTaskT[IO],
        MU,
        TextStyle,
        DownEvent
      ](drawApi.graphics, containerPlacementCurried)
      given RunPlacement[IO, Place] = MeasurableRunPlacement(windowBounds(drawApi.window))
      given ProcessRequest[IO, ApplicationRequest] = ProcessRequestImpl(drawApi.window)

      rootWidget <- rootWidget[widgetApi.type](using widgetApi).runPlacement

      widget <- AtomicCell[IO].of(
        EventConsumerAdapter[IO, Draw[MU, Unit], Place, Recomposition, WidgetTaskImpl[IO, Any], ApplicationRequest, DownEvent](
          Path(List("ROOT")),
          rootWidget,
          taskSet,
          _.traverse_ {
            case RecompositionAction.Task(task) => taskSet.pushTask(task)
            case RecompositionAction.KillTasksFor(path) => taskSet.killTasksFor(path)
          }
        )
      )
      graphics = drawApi.graphics[DrawT[MU]]
      code <- applicationLoop[IO, ApplicationRequest, DownEvent, [A, B] =>> EventConsumerAdapter[IO, DrawT[MU][Unit], Place, Recomposition, WidgetTaskImpl[IO, Any], A, B]](
        eventBus = queue,
        widgetCell = widget,
        drawLoop = currentWidget => drawLoop(drawLoopExceptionHandler, runDraw(graphics.beginDraw), runDraw(graphics.endDraw))(currentWidget.map(_.draw).flatMap(runDraw)),
        updateLoop = updateLoop
      ).flatMap(_.join)
    yield code
  end run

  private def startWidgetTask[F[+_] : Concurrent, T](task: WidgetTaskImpl[F, T], resultDrain: T => F[Unit]): F[Fiber[F, Throwable, Unit]] =
    // TODO add execution context
    Concurrent[F].start(runWidgetTask(task, resultDrain))
  end startWidgetTask

  def offerTask[F[+_]](queue: Queue[F, ? >: TaskFinished], at: Path, taskResult: Any) : F[Unit] =
    queue.offer(TaskFinished(at, taskResult))
  end offerTask

  given LabelPlacement[Place[LayoutPlacementMeta[MU]], TextStyle] with
    override def sizeText(text: String, options: TextStyle): Place[LayoutPlacementMeta[MU]] =
      _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(17))
    end sizeText
  end given
  
  private def containerPlacementCurried[RE]: LayoutPlacement[Update[WidgetTaskImpl[IO, Any]], DrawT[MU][Unit], Place, RE, DownEvent, MU] =
    [Event] => (axis : Axis, elements, main, additional) =>
      weightedRowColumnPlace[MU, widget.Widget[Update[WidgetTaskImpl[IO, Any]], DrawT[MU][Unit], Place, RE, Event, DownEvent]](
        axis,
        elements.map(widget => MaybeWeighted(None, widget)),
        rowColumnPlace(_, _, 
          (elements, bounds) => mainAxisStrategyPlacement[MU](main, elements, bounds.maxValueUnsafe),
          (elements, bounds) => additionalAxisStrategyPlacement[MU](additional, elements, bounds.maxValueUnsafe))
      ).map(unpack)
  end containerPlacementCurried

  private def drawLoopExceptionHandler(exception: Throwable): IO[Option[ExitCode]] =
    IO.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp
