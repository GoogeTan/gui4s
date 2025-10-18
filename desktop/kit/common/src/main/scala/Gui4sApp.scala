package gui4s.desktop.kit
package common

import catnip.syntax.all.given
import cats.arrow.FunctionK
import cats.effect.Async
import cats.effect.kernel.{GenConcurrent, Resource}
import cats.effect.std.QueueSink
import cats.syntax.all.*
import cats.{Functor, Monad, ~>}
import gui4s.core.loop.*
import gui4s.core.widget.Path
import gui4s.core.widget.draw.Drawable
import gui4s.desktop.widget.library.*

import scala.concurrent.ExecutionContext

def gui4sApp[
  IO[_] : Async,
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  DownEvent,
  PreInit,
  Backend,
  ExitCode
](
  createBackend : QueueSink[IO, DownEvent] => Resource[IO, (PreInit, Backend)],
  main : PreInit => Place[Widget[Update, Place, Draw, RecompositionReaction, DownEvent]],
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
    (PreInit, Backend),
    ExitCode
  ](
    backend = createBackend,
    drawLoop = (_, backend) =>
      runDrawLoopOnExecutionContext(
        drawableBasedDrawLoop[IO, Draw, PlacedWidget, ExitCode](
          widgetIsDrawable[Update, Place, Draw, RecompositionReaction, DownEvent],
          runDraw(_, backend)
        ),
        drawLoopExecutionContext
      ),
    updateLoop = (_, backend) => runUpdateLoopOnExecutionContext[IO, PlacedWidget, DownEvent,ExitCode](
      updateLoop[IO, PlacedWidget, DownEvent, ExitCode](
        (widget, event) =>
          flattenRight(
            runUpdate[IO[PlacedWidget]](
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
      ),
  )
end gui4sApp

def flattenRight[F[_] : Monad, A, B](value : F[Either[A, F[B]]]) : F[Either[A, B]] =
  value.flatMap {
    case Left(a)=> Left(a).pure[F]
    case Right(iob) => iob.map(Right(_)) 
  }
end flattenRight

def drawableBasedDrawLoop[
  F[_] : Monad,
  Draw,
  Widget,
  ExitCode
](
  widgetIsDrawable : Drawable[Widget, Draw],
  drawFrame : Draw => F[Unit]
) : DrawLoop[F, Widget, ExitCode] =
  currentWidget =>
    drawLoop(
      currentWidget.flatMap(widget => drawFrame(widgetIsDrawable(widget)))
    )
end drawableBasedDrawLoop
