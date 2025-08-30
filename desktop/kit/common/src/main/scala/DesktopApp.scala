package gui4s.desktop.kit

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import catnip.syntax.functionk.{eitherTMapError, runEitherT}
import cats.*
import cats.data.EitherT
import cats.effect.{ExitCode, IO, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.InfinityOr
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.skija.clear
import gui4s.glfw.*
import effects.*
import gui4s.core.kit.*
import gui4s.core.kit.effects.*

import scala.concurrent.ExecutionContext

def desktopApp[
  IO[_] : Monad,
  PreInit,
  DownEvent
](
  preInit : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent] => Resource[IO, PreInit],
  main : PreInit => DesktopWidget[IO, ApplicationRequest],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
  settings : WindowCreationSettings[Float],
  ffi : ForeignFunctionInterface[IO],
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
    runUpdate = Update.handleApplicationRequests(error => IO.println(error).as(ExitCode.Error)),
    runPlace = backend => Place.run(backend.windowBounds.map(_.map(new InfinityOr(_)))).andThen[EitherT[IO, Throwable, *]](eitherTMapError[IO, String, Throwable](new Exception(_))).andThen(runEitherT[IO, Throwable]),
    runDraw = (draw, backend) => backend.drawFrame(ffi, (clear(ffi) |+| draw).run),
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext
  )
end desktopApp


