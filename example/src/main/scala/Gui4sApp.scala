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

trait Gui4sApp[MeasurementUnit : Fractional] extends IOApp:
  def rootWidget[T <: HighLevelApi & LayoutApi[MeasurementUnit] & LabelApi[Unit]](using api: T) : api.Widget[ApplicationRequest]
  
  type Place[+T] = Measurable[MeasurementUnit, T]
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
          given LayoutDraw[Draw[IO, MeasurementUnit, Unit], LayoutPlacementMeta[MeasurementUnit]] = layoutDrawImpl[DrawT[IO, MeasurementUnit], MeasurementUnit]
          widgetApi = new HighLevelApiImpl[
            Update[Task[Any]],
            Draw[IO, MeasurementUnit, Unit],
            Place,
            Recomposition,
            Task,
            MeasurementUnit,
            TextStyle,
            DownEvent
          ](drawApi.graphics, containerPlacementCurried(ENErrors))
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
          graphics = drawApi.graphics[DrawT[IO, MeasurementUnit]]
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

  def createSwingDrawApi[F[_] : Async](queue : QueueSink[F, DownEvent], impure : Impure[F]) : Resource[F, DrawApi[F, MeasurementUnit]] =
    SwingApi.invoke[F, MeasurementUnit]((frame, windowComponent) => NotifyDrawLoopWindow(SwingWindow(frame, windowComponent, impure), queue.offer(WindowResized)), impure)
  end createSwingDrawApi

  def simpleGraphicsDrawLoop[F[+_] : {Async, Console}, Draw : Monoid](graphics: SimpleDrawApi[MeasurementUnit, Draw], runDraw: Draw => F[Unit]) : DrawLoop[F, Drawable[Draw]] =
    currentWidget =>
      drawLoop(drawLoopExceptionHandler)(
        currentWidget.map(widget => graphics.drawFrame(widget.draw)).flatMap(runDraw)
      )
  end simpleGraphicsDrawLoop

  def runRecompositionInTaskSet[F[_] : Applicative, Task](taskSet: TaskSet[F, Task], recomposition: List[RecompositionAction[Task]]) : F[Unit] =
    recomposition.traverse_(runRecompositionActionInTaskSet(taskSet, _))
  end runRecompositionInTaskSet


  given LabelPlacement[Place[LayoutPlacementMeta[MeasurementUnit]], TextStyle] with
    override def sizeText(text: String, options: TextStyle): Place[LayoutPlacementMeta[MeasurementUnit]] =
      _ => Sized(LayoutPlacementMeta(Fractional[MeasurementUnit].zero, Fractional[MeasurementUnit].zero), Fractional[MeasurementUnit].zero, Fractional[MeasurementUnit].fromInt(17))
    end sizeText
  end given

  // TODO Почему-то ругается на эни в интерполяции строки...
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
    c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp

val ENErrors = MainAxisStrategyErrors(
  "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

