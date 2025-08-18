package me.katze.gui4s.example
package app

import api.effects.{SkijaApplicationRequest, SkijaPlace, SkijaRecomposition, SkijaUpdate}
import loop.*
import skija.{SkijaBackend, skijaDrawLoop}

import cats.{Functor, Monad}
import cats.effect.{Async, ExitCode}
import cats.effect.kernel.Resource
import cats.effect.std.{Console, QueueSink}
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.glfw.OglGlfwWindow
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.*

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext

def desktopApp[
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
  main : (PreInit, Backend) => Place[Widget[Update, Place, Draw, RecompositionReaction, DownEvent]],
  shouldDrawLoopContinueDrawing : Backend => IO[Boolean],
  runUpdate : [T] => Update[T] => IO[Either[ExitCode, T]],
  runPlace : Backend => [T] => Place[T] => IO[T],
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
    backend = createBackend,
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
              runPlace(backend),
            )(widget, event)
          )
      ),
      updateLoopExecutionContext
    ),
    rootWidget = (preInit, backend) =>
      placeForTheFirstTime[IO, PlacedWidget, Place, RecompositionReaction](
        Path(Nil),
        main(preInit, backend),
        widgetReactsOnRecomposition,
        runRecomposition,
        runPlace(backend)
      )
  )
end desktopApp
