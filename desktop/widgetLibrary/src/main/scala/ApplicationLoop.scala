package gui4s.desktop.widget.library

import cats.*
import cats.effect.*
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import gui4s.core.loop.*
import scala.concurrent.ExecutionContext
import gui4s.core.widget.draw.Drawable
import gui4s.core.widget.Path

def widgetLoops[
  IO[_] : Async,
  CallbackIO[_] : Concurrent,
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  DownEvent,
  ExitCode
](
   waitForTheNextEvent : IO[DownEvent],
   widget : Ref[IO, Widget[Update, Place, Draw, RecompositionReaction, DownEvent]],
   runUpdate : [T] => Update[T] => IO[Either[ExitCode, T]],
   runPlace : Place ~> IO,
   runDraw : Draw => IO[Boolean],
   runRecomposition : RecompositionReaction => IO[Unit],
   drawLoopExecutionContext : ExecutionContext,
   updateLoopExecutionContext : ExecutionContext,
   successExitCode : ExitCode
 ) : IO[ExitCode] =
  type PlacedWidget = Widget[
    Update, Place, Draw, RecompositionReaction, DownEvent
  ]
  applicationLoop(
    waitForTheNextEvent,
    widget,
    runDrawLoopOnExecutionContext(
      drawableBasedDrawLoop[IO, Draw, PlacedWidget, ExitCode](
        widgetIsDrawable[Update, Place, Draw, RecompositionReaction, DownEvent],
        runDraw,
        successExitCode
      ),
      drawLoopExecutionContext
    ),
    runUpdateLoopOnExecutionContext[IO, PlacedWidget, DownEvent,ExitCode](
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
                runPlace,
              )(widget, event)
            )
          )
      ),
      updateLoopExecutionContext
    )
  )
end widgetLoops

def flattenRight[F[_] : Monad, A, B](value : F[Either[A, F[B]]]) : F[Either[A, B]] =
  value.flatMap {
    case Left(a) => Left(a).pure[F]
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
   drawFrame : Draw => F[Boolean],
   success : ExitCode
 ) : DrawLoop[F, Widget, ExitCode] =
  currentWidget =>
    drawLoop(
      success
    )(
      currentWidget.flatMap(widget => drawFrame(widgetIsDrawable(widget))),
    )
end drawableBasedDrawLoop
