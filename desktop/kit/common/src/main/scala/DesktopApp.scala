package gui4s.desktop.kit
package common

import common.effects.*
import common.effects.Place.given
import common.effects.Draw.given
import common.widgets.DesktopWidget
import catnip.syntax.all.given
import catnip.syntax.functionk.runEitherT
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.InfinityOr
import gui4s.desktop.skija.canvas.clear
import glfw4s.core.*
import glfw4s.core.pure.PostInit
import io.github.humbleui.skija.Canvas

import scala.concurrent.ExecutionContext

def desktopApp[
  IO[_] : Async,
  Resource[_] : Monad,
  CallbackIO[_] : Concurrent,
  Monitor,
  Window,
  PreInit,
](
  preInit : SkijaBackend[IO, Resource, CallbackIO, Monitor, Window, DownEvent] => Resource[PreInit],
  main : PreInit => DesktopWidget[IO, ApplicationRequest],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
  settings : WindowCreationSettings[Monitor, Window],
  unsafeRunF : IO[Unit] => Unit,
  glfw : PostInit[IO, Resource, CallbackIO[Unit], Monitor, Window],
  eval : IO ~> Resource,
  fromAutoCloseable : [T <: AutoCloseable] => CallbackIO[T] => Resource[T],
  liftIO: CallbackIO ~> IO
)(using GenConcurrent[CallbackIO, ?]) : IO[ExitCode] =
  gui4sApp[
    IO,
    CallbackIO,
    Resource,
    Update[IO, ApplicationRequest, *],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    PreInit,
    SkijaBackend[IO, Resource, CallbackIO, Monitor, Window, DownEvent],
    ExitCode
  ](
    queue =>
      for
        backend <-
          SkijaBackend.create(
            queue,
            glfw,
            settings,
            DownEvent.eventOfferingCallbacks(queue.offer),
            eval,
            fromAutoCloseable,
            liftIO
          )
        preInit <- preInit(backend)
      yield (preInit, backend),
    main = main,
    runUpdate = Update.handleApplicationRequests(MonadThrow[IO].raiseError),
    runPlace = backend =>
      Place.run(backend.windowBounds.map(_.map(new InfinityOr(_))))
        .andThen(runEitherT[IO, Throwable]),
    runDraw = (draw, backend) => backend.drawFrame((clear[ReaderT[CallbackIO, Canvas, *]](0xFFFFFFFF) |+| draw).run),
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext,
    liftQueueIO = liftIO
  )
end desktopApp


