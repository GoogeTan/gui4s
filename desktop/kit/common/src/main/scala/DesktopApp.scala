package gui4s.desktop.kit
package common

import common.effects.*
import common.effects.Place.given
import common.widgets.DesktopWidget

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import catnip.syntax.functionk.runEitherT
import cats.*
import cats.data.EitherT
import cats.effect.{Async, ExitCode, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.InfinityOr
import gui4s.desktop.skija.clear
import gui4s.glfw.*

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
  unsafeRunF : IO[Unit] => Unit,
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
            DownEvent.eventOfferingCallbacks(queue.offer),
            unsafeRunF
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


