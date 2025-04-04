package me.katze.gui4s.example

import api.impl.{HighLevelApiImpl, LayoutPlacement, LayoutPlacementMeta}
import api.{HighLevelApi, LabelApi, LayoutApi}
import draw.*
import draw.swing.{SwingApi, SwingDraw, SwingDrawT, runSwingDraw, given}
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
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{LayoutDraw, LiftEventReaction, TextPlacement}
import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents, KillTasks, Path, RaiseEvent}
import me.katze.gui4s.widget.{EventResult, given}

trait Gui4sApp[
  Place[+_] : FlatMap,
  Update[+_, +_] : {BiMonad, CatchEvents, RaiseEvent},
  Recomposition : {KillTasks, Monoid},
  Task[+_],
  MeasurementUnit : Fractional,
  Draw : Monoid,
  TextStyle
](
    val drawApi: QueueSink[IO, DownEvent] => Resource[IO, DrawApi[IO, MeasurementUnit, Draw]],
    val runDraw : Draw => IO[Unit],
    val containerPlacement : LayoutPlacement[Update, Draw, Place, Recomposition, DownEvent, MeasurementUnit],
    val runPlacement : IO[Bounds[MeasurementUnit]] => RunPlacement[IO, Place],
    val runRecompositionInTaskSet : (TaskSet[IO, Task[Any]], Recomposition) => IO[Unit]
)(
  using
  LayoutDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
  TextPlacement[Place[LayoutPlacementMeta[MeasurementUnit]], TextStyle],
  LiftEventReaction[Update, Task[Any]]
) extends IOApp:
  def rootWidget[T <: HighLevelApi & LayoutApi[MeasurementUnit] & LabelApi[TextStyle]](using api: T) : api.Widget[ApplicationRequest]
  
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final override def run(args: List[String]): IO[ExitCode] =
    for
      queue <- Queue.unbounded[IO, DownEvent]
      code <- drawApi(queue).use(drawApi =>
        for
          taskSet <- runInQueueTaskSet(queue, IOImpure)
          widgetApi = new HighLevelApiImpl[
            Update,
            Draw,
            Place,
            Recomposition,
            Task,
            MeasurementUnit,
            TextStyle,
            DownEvent
          ](drawApi.graphics, containerPlacement) //containerPlacementCurried(ENErrors))
          given RunPlacement[IO, Place] = runPlacement(drawApi.windowBounds)
          given ProcessRequest[IO, ApplicationRequest] = ProcessRequestImpl[IO]()

          rootWidget <- rootWidget[widgetApi.type](using widgetApi).runPlacement

          widget <- Ref[IO].of(
            EventConsumerAdapter(
              pathToRoot = Path(List("ROOT")),
              rootWidget,
              taskSet = taskSet,
              runRecomposition = runRecompositionInTaskSet(taskSet, _)
            )
          )
          code <- applicationLoop(
            eventBus = queue,
            widgetCell = widget,
            drawLoop = simpleGraphicsDrawLoop(drawApi.graphics, runDraw),
            updateLoop = updateLoop
          ).flatMap(_.join)
        yield code
      )
    yield code
  end run

  def simpleGraphicsDrawLoop[F[+_] : {Async, Console}](graphics: SimpleDrawApi[MeasurementUnit, Draw], runDraw: Draw => F[Unit]) : DrawLoop[F, Drawable[Draw]] =
    currentWidget =>
      drawLoop(drawLoopExceptionHandler)(
        currentWidget.map(widget => graphics.drawFrame(widget.draw)).flatMap(runDraw)
      )
  end simpleGraphicsDrawLoop


  // TODO Почему-то ругается на эни в интерполяции строки...
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
    c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
  end drawLoopExceptionHandler
end Gui4sApp


