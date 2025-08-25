package gui4s.desktop.kit.cats

import catnip.syntax.all.given
import cats.arrow.FunctionK
import cats.effect.kernel.Resource
import cats.effect.std.{Console, QueueSink}
import cats.effect.{Async, ExitCode}
import cats.syntax.all.*
import cats.{Functor, Monad, MonadError}
import gui4s.core.loops.*
import gui4s.core.widget.Path
import gui4s.core.widget.draw.Drawable
import gui4s.decktop.widget.library.*

import scala.concurrent.ExecutionContext

def gui4sApp[
  IO[_] : {Async, Console},
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  DownEvent,
  PreInit,
  Backend
](
  createBackend : QueueSink[IO, DownEvent] => Resource[IO, (PreInit, Backend)],
  main : PreInit => Place[Widget[Update, Place, Draw, RecompositionReaction, DownEvent]],
  shouldDrawLoopContinueDrawing : Backend => IO[Boolean],
  runUpdate : [T] => Update[T] => IO[Either[ExitCode, T]],
  runPlace : Backend => FunctionK[Place, IO],
  runDraw : (Draw, Backend) => IO[Unit],
  runRecomposition : RecompositionReaction => IO[Unit],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
) : IO[ExitCode] =
  type PlacedWidget = Widget[
    Update, Place, Draw, RecompositionReaction, DownEvent
  ]

  runApplicationLoopsWithBackend[
    IO,
    DownEvent,
    PlacedWidget,
    (PreInit, Backend)
  ](
    backend = queue => createBackend(queue).evalOn(drawLoopExecutionContext),
    drawLoop = (_, backend) =>
      runDrawLoopOnExecutionContext(
        skijaDrawLoop[IO, Draw, PlacedWidget](
          widgetIsDrawable[Update, Place, Draw, RecompositionReaction, DownEvent],
          shouldDrawLoopContinueDrawing(backend),
          runDraw(_, backend)
        ),
        drawLoopExecutionContext
      ),
    updateLoop = (_, backend) => runUpdateLoopOnExecutionContext[IO, PlacedWidget, DownEvent](
      updateLoop(
        (widget, event) =>
          runUpdate(
            processEvent[
              IO,
              PlacedWidget,
              Place,
              Update,
              RecompositionReaction,
              DownEvent
            ](
              Path(Nil),
              runRecomposition,
              widgetHandlesEvent,
              widgetReactsOnRecomposition,
              widgetHasInnerStates,
              runPlace(backend).convert,
            )(widget, event)
          )
      ),
      updateLoopExecutionContext
    ),
    rootWidget = (preInit, backend) =>
      placeForTheFirstTime[IO, PlacedWidget, Place, RecompositionReaction](
        Path(Nil),
        main(preInit),
        widgetReactsOnRecomposition,
        runRecomposition,
        runPlace(backend).convert
      )
  )
end gui4sApp

// TODO rename me
def skijaDrawLoop[
  F[_] : Console,
  Draw,
  Widget
](
  widgetIsDrawable : Drawable[Widget, Draw],
  shouldContinue : F[Boolean],
  drawFrame : Draw => F[Unit]
)(using MonadError[F, Throwable]) : DrawLoop[F, Widget] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, shouldContinue)(
      currentWidget.flatMap(widget => drawFrame(widgetIsDrawable(widget)))
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop

def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
  c.println[String]("Error in draw loop: " + exception.toString).map(_ => Some(ExitCode.Error))
end drawLoopExceptionHandler
