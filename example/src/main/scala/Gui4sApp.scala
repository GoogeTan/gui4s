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
import cats.effect.std.{AtomicCell, Console, Queue}
import cats.syntax.all.*
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

  type Recomposition = List[RecompositionAction[RunnableIO[EventProducingEffectT[IO], Any]]]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final override def run(args: List[String]): IO[ExitCode] =
    given KillTasks[Recomposition] = path => List(RecompositionAction.KillTasksFor(path))

    for
      queue <- Queue.unbounded[IO, DownEvent]
      (drawApi, destroyDrawApi) <- SwingApi.invoke[MU]((frame, windowComponent) => NotifyDrawLoopWindow(SwingWindow(frame, windowComponent), queue.offer(WindowResized))).allocated
      taskSet <- runInQueueTaskSet(queue, IOImpure)
      given LayoutDraw[Draw[MU, Unit], LayoutPlacementMeta[MU]] = layoutDrawImpl[DrawT[MU], MU]
      widgetApi = new HighLevelApiImpl[
        Update[RunnableIO[EventProducingEffectT[IO], Any]],
        Draw[MU, Unit],
        Place,
        Recomposition,
        [T] =>> RunnableIO[EventProducingEffectT[IO], T],
        MU,
        TextStyle,
        DownEvent
      ](drawApi.graphics, containerPlacementCurried)
      given RunPlacement[IO, Place] = MeasurableRunPlacement(windowBounds(drawApi.window))
      given ProcessRequest[IO, ApplicationRequest] = ProcessRequestImpl(drawApi.window)

      rootWidget <- rootWidget[widgetApi.type](using widgetApi).runPlacement

      widget <- AtomicCell[IO].of(
        EventConsumerAdapter(
          pathToRoot = Path(List("ROOT")),
          rootWidget,
          taskSet = taskSet,
          runRecomposition = runRecompositionInTaskSet(taskSet, _)
        )
      )
      graphics = drawApi.graphics[DrawT[MU]]
      code <- applicationLoop(
        eventBus = queue,
        widgetCell = widget,
        drawLoop = simpleGraphicsDrawLoop(graphics, runDraw),
        updateLoop = updateLoop
      ).flatMap(_.join)
    yield code
  end run

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