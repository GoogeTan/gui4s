package gui4s.desktop.kit

import widgets.DesktopWidget

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import catnip.syntax.functionk.{eitherTMapError, runEitherT}
import cats.*
import cats.data.EitherT
import cats.effect.{ExitCode, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.InfinityOr
import effects.*
import effects.Place.given 

import gui4s.desktop.kit.*

import gui4s.desktop.skija.clear
import gui4s.glfw.*
import cats.effect.Async
import cats.effect.std.Console

import scala.concurrent.ExecutionContext

def desktopApp[
  IO[_] : {Async, ForeignFunctionInterface as ffi},
  PreInit,
](
  preInit : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent] => Resource[IO, PreInit],
  main : PreInit => DesktopWidget[IO, ApplicationRequest],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
  settings : WindowCreationSettings[Float],
) : IO[ExitCode] =
  gui4sApp[
    IO,
    Update[IO, ApplicationRequest, *],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    PreInit,
    SkijaBackend[IO, Long, OglGlfwWindow, DownEvent],
    ExitCode
  ](
    queue =>
      for
        backend <-
          SkijaBackend.create(
            queue,
            settings,
            ffi,
            DownEvent.eventOfferingCallbacks(queue.offer)
          )
        preInit <- preInit(backend)
      yield (preInit, backend),
    main = main,
    runUpdate = Update.handleApplicationRequests(MonadThrow[IO].raiseError),
    runPlace = backend =>
      Place.run(backend.windowBounds.map(_.map(new InfinityOr(_))))
        .andThen(runEitherT[IO, Throwable]),
    runDraw = (draw, backend) => backend.drawFrame(ffi, (clear(ffi) |+| draw).run),
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext
  )
end desktopApp


