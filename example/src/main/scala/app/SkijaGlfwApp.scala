package me.katze.gui4s.example
package app

import api.effects.{*, given}
import loop.{runApplicationLoopsWithBackend, runDrawLoopOnExecutionContext, runUpdateLoopOnExecutionContext, updateLoop}
import skija.SkijaBackend.GlfwCallbacks
import skija.{SkijaBackend, skijaDrawLoop}

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.EitherT
import cats.effect.std.{Console, QueueSink}
import cats.effect.{Async, ExitCode, Resource}
import cats.syntax.all.*
import me.katze.*
import me.katze.gui4s.example
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.glfw.*
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.*

import scala.concurrent.ExecutionContext

def skijaGlfwApp[
  IO[_] : {Async, Console},
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  DownEvent,
  PreInit,
](
  preInit : QueueSink[IO, DownEvent] => Resource[IO, PreInit],
  main : (PreInit, SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) => Place[Widget[Update, Place, Draw, RecompositionReaction, DownEvent]],
  runUpdate : [T] => Update[T] => IO[Either[ExitCode, T]],
  runPlace : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent] => [T] => Place[T] => IO[T],
  runDraw : (Draw, SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) => IO[Unit],
  runRecomposition : RecompositionReaction => IO[Unit],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
  settings : WindowCreationSettings[Float],
  ffi : ForeighFunctionInterface[IO],
  callbacks : QueueSink[IO, DownEvent] => GlfwCallbacks[IO[Unit], Float],
) : IO[ExitCode] =
  desktopApp[
    IO,
    Update,
    Place,
    Draw,
    RecompositionReaction,
    DownEvent,
    PreInit,
    SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]
  ](
    queue => preInit(queue).product(
      SkijaBackend.createForTestsTrue(
        queue,
        settings,
        ffi,
        callbacks(queue)
      )
    ),
    main,
    _.windowShouldNotClose,
    runUpdate,
    runPlace,
    runDraw,
    runRecomposition,
    drawLoopExecutionContext,
    updateLoopExecutionContext
  )
end skijaGlfwApp

