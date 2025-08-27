package gui4s.desktop.kit.cats

import effects.*
import effects.Place.given
import effects.Update.given

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import catnip.syntax.functionk.{eitherTMapError, runEitherT}
import cats.*
import cats.data.EitherT
import cats.effect.{ExitCode, IO, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.InfinityOr
import gui4s.glfw.*
import gui4s.desktop.skija.clear

import widgets.DesktopWidget
import scala.concurrent.ExecutionContext

def desktopApp[
  PreInit,
](
  preInit : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent] => Resource[IO, PreInit],
  main : PreInit => DesktopWidget[ApplicationRequest],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
  settings : WindowCreationSettings[Float],
  ffi : ForeignFunctionInterface[IO],
) : IO[ExitCode] =
  gui4sApp[
    IO,
    Update[ApplicationRequest, *],
    Place,
    Draw,
    RecompositionReaction,
    DownEvent,
    PreInit,
    SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]
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
    main,
    _.windowShouldNotClose,
    Update.handleApplicationRequests(error => IO.println(error).as(ExitCode.Error)),
    backend => Place.run(backend.windowBounds.map(_.map(new InfinityOr(_)))).andThen[EitherT[IO, Throwable, *]](eitherTMapError[IO, String, Throwable](new Exception(_))).andThen(runEitherT[IO, Throwable]),
    (draw, backend) => backend.drawFrame(ffi, (clear(ffi) |+| draw).run),
    RecompositionReaction.run,
    drawLoopExecutionContext,
    updateLoopExecutionContext
  )
end desktopApp


