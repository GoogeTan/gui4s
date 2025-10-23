package gui4s.desktop.kit
package common

import common.effects.*
import common.effects.Place.given
import common.effects.Draw.given
import common.widgets.DesktopWidget
import catnip.syntax.functionk.runEitherT
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import catnip.syntax.all.*
import catnip.resource.*
import gui4s.core.geometry.InfinityOr
import gui4s.desktop.skija.canvas.clear
import glfw4s.core.*
import glfw4s.core.pure.PostInit
import io.github.humbleui.skija.Canvas

import scala.concurrent.ExecutionContext

def desktopApp[
  IO[_] : Async,
  Resource[_] : { Async, MakeC[CallbackIO], AllocateC[CallbackIO], EvalC[IO], EvalC[CallbackIO], UseC[IO], SyncResource },
  CallbackIO[_] : Async,
  Monitor,
  Window,
  PreInit,
](
  preInit : SkijaBackend[IO, Resource, CallbackIO, Monitor, Window, DownEvent] => Resource[PreInit],
  main : PreInit => DesktopWidget[IO, ApplicationRequest],
  drawLoopExecutionContext : ExecutionContext,
  updateLoopExecutionContext : ExecutionContext,
  settings : WindowCreationSettings[Monitor, Window],
  glfw : PostInit[IO, Resource, CallbackIO[Unit], Monitor, Window],
  liftIO : CallbackIO ~> IO,
) : IO[ExitCode] =
  gui4sApp[
    IO,
    Resource,
    CallbackIO,
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
      SkijaBackend.create[IO, Resource, CallbackIO, Monitor, Window, DownEvent](
        queue,
        glfw,
        glfw.createWindow(settings) <*< glfw.makeContextCurrent,
        window => glfw.getFramebufferSize(window).eval.flatMap(
          (width, height) => SkijaBackend.createRenderTarget(width, height)
        ),
        liftIO,
      ).sigmaProduct(preInit),
    main = main,
    runUpdate = Update.handleApplicationRequests(MonadThrow[IO].raiseError),
    runPlace = backend =>
      Place.run(backend.windowBounds.map(_.map(new InfinityOr(_))))
        .andThen(runEitherT[IO, Throwable]),
    runDraw = (draw, backend) => backend.drawFrame(clear[ReaderT[IO, Canvas, *]](0xFFFFFFFF) |+| draw),
    runRecomposition = RecompositionReaction.run,
    drawLoopExecutionContext = drawLoopExecutionContext,
    updateLoopExecutionContext = updateLoopExecutionContext,
    liftIO = liftIO,
  )
end desktopApp


