package me.katze.gui4s.example

import api.impl.{HighLevelApiImpl, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.*
import draw.swing.{SwingApi, SwingWindow}
import impl.{*, given}
import place.*
import task.*
import update.*

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.{Console, Queue, QueueSink}
import cats.syntax.all.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.stateful.{KillTasks, Path}
import me.katze.gui4s.widget.{EventResult, given}

trait Gui4sApp[MU : Fractional] extends IOApp:
  def rootWidget[T <: HighLevelApi & LayoutApi[MU] & LabelApi[Unit]](using api: T) : api.Widget[ApplicationRequest]
  
  type Place[+T] = Measurable[MU, T]
  type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
  type TextStyle = Unit
  type Task[T] = RunnableIO[EventProducingEffectT[IO], T]

  type Recomposition = List[RecompositionAction[RunnableIO[EventProducingEffectT[IO], Any]]]
  given KillTasks[Recomposition] = path => List(RecompositionAction.KillTasksFor(path))

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final override def run(args: List[String]): IO[ExitCode] =
    for
      queue <- Queue.unbounded[IO, DownEvent]
      code <- createSwingDrawApi(queue, IOImpure).use(drawApi =>
        for
          taskSet <- runInQueueTaskSet(queue, IOImpure)
          given LayoutDraw[Draw[IO, MU, Unit], LayoutPlacementMeta[MU]] = layoutDrawImpl[DrawT[IO, MU], MU]
          widgetApi = new HighLevelApiImpl[
            Update[Task[Any]],
            Draw[IO, MU, Unit],
            Place,
            Recomposition,
            Task,
            MU,
            TextStyle,
            DownEvent
          ](drawApi.graphics, containerPlacementCurried)
          given RunPlacement[IO, Place] = MeasurableRunPlacement(windowBounds(drawApi.window))
          given ProcessRequest[IO, ApplicationRequest] = ProcessRequestImpl(drawApi.window)

          rootWidget <- rootWidget[widgetApi.type](using widgetApi).runPlacement

          widget <- Ref[IO].of(
            EventConsumerAdapter(
              pathToRoot = Path(List("ROOT")),
              rootWidget,
              taskSet = taskSet,
              runRecomposition = runRecompositionInTaskSet(taskSet, _)
            )
          )
          graphics = drawApi.graphics[DrawT[IO, MU]]
          code <- applicationLoop(
            eventBus = queue,
            widgetCell = widget,
            drawLoop = simpleGraphicsDrawLoop(graphics, runDraw),
            updateLoop = updateLoop
          ).flatMap(_.join)
        yield code
      )
    yield code
  end run

  def createSwingDrawApi[F[_] : Async](queue : QueueSink[F, DownEvent], impure : Impure[F]) : Resource[F, DrawApi[F, MU]] =
    SwingApi.invoke[F, MU]((frame, windowComponent) => NotifyDrawLoopWindow(SwingWindow(frame, windowComponent, impure), queue.offer(WindowResized)), impure)
  end createSwingDrawApi

  def simpleGraphicsDrawLoop[F[+_] : Async : Console, Draw : Monoid](graphics: SimpleDrawApi[MU, Draw], runDraw: Draw => F[Unit]) : DrawLoop[F, Drawable[Draw]] =
    currentWidget =>
      drawLoop(drawLoopExceptionHandler)(
        currentWidget.map(widget => graphics.drawFrame(widget.draw)).flatMap(runDraw)
      )
  end simpleGraphicsDrawLoop

  def runRecompositionInTaskSet[F[_] : Applicative, Task](taskSet: TaskSet[F, Task], recomposition: List[RecompositionAction[Task]]) : F[Unit] =
    recomposition.traverse_(runRecompositionActionInTaskSet(taskSet, _))
  end runRecompositionInTaskSet


  given LabelPlacement[Place[LayoutPlacementMeta[MU]], TextStyle] with
    override def sizeText(text: String, options: TextStyle): Place[LayoutPlacementMeta[MU]] =
      _ => Sized(LayoutPlacementMeta(Fractional[MU].zero, Fractional[MU].zero), Fractional[MU].zero, Fractional[MU].fromInt(17))
    end sizeText
  end given

  private def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
    c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp