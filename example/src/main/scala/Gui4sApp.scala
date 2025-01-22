package me.katze.gui4s.example

import api.impl.{DrawMonad, HighLevelApiImpl, LayoutPlacement, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.{Drawable, NotifyDrawLoopWindow, ProcessRequestImpl, SimpleDrawApi, windowBounds}
import place.{RunPlacement, additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}
import task.{ContramapTaskSet, IOOnThread, RefTaskSet, RunnableIO, TaskSet, WidgetTaskImpl}
import update.{ApplicationRequest, CatsFiber, MultiMap, ProcessRequest, StandardMapWrapperMultiMap}

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.*
import me.katze.gui4s.widget.library.*
import cats.effect.std.{AtomicCell, Console, Queue, QueueSink}
import draw.swing.{SwingApi, SwingWindow}

import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.given
import me.katze.gui4s.widget.{EventResult, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.stateful.{EventReaction, KillTasks, Path, TaskFinished}

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
  
  given runnableLiftEventReaction[Task] : LiftEventReaction[Update[Task], Task] with
    override def lift[A, B](reaction: EventReaction[Task, A, B]): EventResult[Task, A, B] =
      EventResult(
        reaction.newState, 
        reaction.parentEvent,
        reaction.ios
      )
    end lift
  end runnableLiftEventReaction
  
  private def runDraw(draw: DrawT[MU][Unit]) : IO[Unit] =
    draw.run(Numeric[MU].zero, Numeric[MU].zero)
  end runDraw
  
  enum RecompositionAction[Task]:
    case Task(task : Task)
    case KillTasksFor(path : Path)
  end RecompositionAction

  def runInQueueTaskSet(taskMap : AtomicCell[IO, MultiMap[Path, IOOnThread[CatsFiber[IO, Throwable, Any]]]], queue : QueueSink[IO, DownEvent]) : TaskSet[IO, RunnableIO[WidgetTaskT[IO], Any]] =
    ContramapTaskSet(
      RefTaskSet[IO, RunnableIO[IO, Any], CatsFiber[IO, Throwable, Any]](
        taskMap
      ),
      a => RunnableIO(a.io(offerTask(queue, a.owner, _ : Any)), a.owner, a.keepAliveAfterOwnerDetach, IOImpure)
    )
  end runInQueueTaskSet

  type Recomposition = List[RecompositionAction[RunnableIO[WidgetTaskT[IO], Any]]]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final override def run(args: List[String]): IO[ExitCode] =
    given KillTasks[Recomposition] = path => List(RecompositionAction.KillTasksFor(path))

    for
      queue <- Queue.unbounded[IO, DownEvent]
      (drawApi, destroyDrawApi) <- SwingApi.invoke[MU]((frame, windowComponent) => NotifyDrawLoopWindow(SwingWindow(frame, windowComponent), queue.offer(WindowResized))).allocated
      taskMap <- AtomicCell[IO].of[MultiMap[Path, IOOnThread[CatsFiber[IO, Throwable, Any]]]](new StandardMapWrapperMultiMap())
      taskSet = runInQueueTaskSet(taskMap, queue)
      widgetApi = new HighLevelApiImpl[
        Update[RunnableIO[WidgetTaskT[IO], Any]],
        Draw[MU, Unit],
        Place,
        Recomposition,
        [T] =>> RunnableIO[WidgetTaskT[IO], T],
        MU,
        TextStyle,
        DownEvent
      ](drawApi.graphics, containerPlacementCurried)
      given RunPlacement[IO, Place] = MeasurableRunPlacement(windowBounds(drawApi.window))
      given ProcessRequest[IO, ApplicationRequest] = ProcessRequestImpl(drawApi.window)

      rootWidget <- rootWidget[widgetApi.type](using widgetApi).runPlacement

      widget <- AtomicCell[IO].of(
        EventConsumerAdapter[IO, Draw[MU, Unit], Place, Recomposition, RunnableIO[WidgetTaskT[IO], Any], ApplicationRequest, DownEvent](
          Path(List("ROOT")),
          rootWidget,
          taskSet,
          runRecompositionInTaskSet(taskSet, _)
        )
      )
      graphics = drawApi.graphics[DrawT[MU]]
      code <- applicationLoop[IO, ApplicationRequest, DownEvent, [A, B] =>> EventConsumerAdapter[IO, DrawT[MU][Unit], Place, Recomposition, RunnableIO[WidgetTaskT[IO], Any], A, B]](
        eventBus = queue,
        widgetCell = widget,
        drawLoop = simpleGraphicsDrawLoop(graphics, runDraw),
        updateLoop = updateLoop
      ).flatMap(_.join)
    yield code
  end run

  def simpleGraphicsDrawLoop[F[+_] : Async : Console, Draw : Monoid](graphics: SimpleDrawApi[MU, Draw], runDraw: Draw => F[Unit]) : DrawLoop[F, Drawable[Draw]] =
    currentWidget =>
      drawLoop(drawLoopExceptionHandler)(currentWidget.map(widget => graphics.beginDraw |+| widget.draw |+| graphics.endDraw).flatMap(runDraw))
  end simpleGraphicsDrawLoop

  def runRecompositionInTaskSet[F[+_] : Applicative, Task](taskSet: TaskSet[F, Task], recomposition: List[RecompositionAction[Task]]) : F[Unit] =
    recomposition.traverse_(runRecompositionActionInTaskSet(taskSet, _))
  end runRecompositionInTaskSet

  def runRecompositionActionInTaskSet[F[+_], Task](taskSet: TaskSet[F, Task], recompositionAction: RecompositionAction[Task]) : F[Unit] =
    recompositionAction match
      case RecompositionAction.Task(task) => taskSet.pushTask(task)
      case RecompositionAction.KillTasksFor(path) => taskSet.killTasksFor(path)
    end match
  end runRecompositionActionInTaskSet

  def offerTask[F[+_]](queue: QueueSink[F, ? >: TaskFinished], at: Path, taskResult: Any) : F[Unit] =
    queue.offer(TaskFinished(at, taskResult))
  end offerTask

  given LabelPlacement[Place[LayoutPlacementMeta[MU]], TextStyle] with
    override def sizeText(text: String, options: TextStyle): Place[LayoutPlacementMeta[MU]] =
      _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(17))
    end sizeText
  end given
  
  private def containerPlacementCurried[RE]: LayoutPlacement[Update[RunnableIO[WidgetTaskT[IO], Any]], DrawT[MU][Unit], Place, RE, DownEvent, MU] =
    [Event] => (axis : Axis, elements, main, additional) =>
      weightedRowColumnPlace[MU, widget.Widget[Update[RunnableIO[WidgetTaskT[IO], Any]], DrawT[MU][Unit], Place, RE, Event, DownEvent]](
        axis,
        elements.map(widget => MaybeWeighted(None, widget)),
        rowColumnPlace(_, _, 
          (elements, bounds) => mainAxisStrategyPlacement[MU](main, elements, bounds.maxValueUnsafe),
          (elements, bounds) => additionalAxisStrategyPlacement[MU](additional, elements, bounds.maxValueUnsafe))
      ).map(unpack)
  end containerPlacementCurried

  private def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
    c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp
