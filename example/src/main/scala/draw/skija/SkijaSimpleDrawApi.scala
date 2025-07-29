package me.katze.gui4s.example
package draw.skija

import draw.{Drawable, drawLoopExceptionHandler}

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.effect.std.{AtomicCell, Console, Dispatcher, QueueSink, Supervisor}
import cats.effect.{Async, Clock, Concurrent, ExitCode, Resource}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.{Apply, MonadError, Monoid}
import me.katze.gui4s.geometry.{Point2d, Rect}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.*
import org.lwjgl.opengl.GL.createCapabilities
import scalacache.caffeine.CaffeineCache

import scala.annotation.experimental

@experimental
object SkijaSimpleDrawApi:
  final case class GlfwCallbacks[
    F,
    MeasurementUnit
  ](
    onWindowResized: (newSize : Rect[MeasurementUnit]) => F,
    onMouseClick: (Int, KeyAction, KeyModes) => F,
    onMouseMove: Point2d[MeasurementUnit] => F,
    onKeyPress: (Int, Int, KeyAction, KeyModes) => F,
    onScroll : (xoffset : MeasurementUnit, yoffset : MeasurementUnit) => F
  )

  def createForTestsTrue[
    F[+_] : {Async, Console}, DownEvent
  ](
     queue : QueueSink[F, DownEvent],
     settings : WindowCreationSettings[Float],
     ffi: ForeighFunctionInterface[F],
     callbacks : GlfwCallbacks[F[Unit], Float],
    ): Resource[F, SkijaBackend[F, Long, GlfwWindow[F, Long, Float], DownEvent]] =
    for
      skija <- Resource.eval(SkijaInitImpl(ffi))
      dispatcher <- Dispatcher.sequential[F]
      supervisor <- Supervisor[F]
      glfw: Glfw[F, Long, OglGlfwWindow[F]] <- GlfwImpl[F](dispatcher)(using ffi)
      res <- createForTests(queue, glfw, ffi(createCapabilities()), skija, dispatcher, supervisor, settings, callbacks)
    yield res
  end createForTestsTrue

  def createForTests[
    F[+_] : {Async, Console},
    Monitor, 
    Window <: me.katze.gui4s.glfw.GlfwWindow[F, Monitor, Float],
    DownEvent
  ](
     queue : QueueSink[F, DownEvent],
     glfw : Glfw[F, Monitor, Window],
     createGlCapabilities : F[Unit],
     skija : SkijaInit[F],
     dispatcher : Dispatcher[F],
     supervisor : Supervisor[F],
     windowSettings : WindowCreationSettings[Float],
     callbacks : GlfwCallbacks[F[Unit], Float],
    ): Resource[F, SkijaBackend[F, Monitor, Window, DownEvent]] =
    for
      window <- glfw.createWindow(windowSettings)
      _ <- Resource.eval(glfw.createOGLContext(window) *> createGlCapabilities)
      renderTargetCell <- createRenderTarget(glfw, skija, windowSettings.size)
      _ <- Resource.eval(
        registerCallbacks[F, Monitor, Window, Float](
          glfw,
          window,
          addRenderTargetRecreation[F[Unit], Float](
            callbacks,
            recreateRenderTarget(skija, renderTargetCell, _)
          )
        )
      )
      shaper <- skija.createShaper
      cache <- Resource.eval(CaffeineCache[F, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]])
    yield SkijaBackend[F, Monitor, Window, DownEvent](queue, glfw, window, renderTargetCell, dispatcher, supervisor, shaper, cache)
  end createForTests

  def addRenderTargetRecreation[F : Monoid, MeasurementUnit](
                                                              callbacks: GlfwCallbacks[F, MeasurementUnit],
                                                              recreation : Rect[MeasurementUnit] => F
                                                            ) : GlfwCallbacks[F, MeasurementUnit] =
    callbacks.copy(
      onWindowResized = newSize =>
        recreation(newSize) |+| callbacks.onWindowResized(newSize)
    )
  end addRenderTargetRecreation


  def createRenderTarget[F[_] : Concurrent, Monitor, Window](
                                                              glfw : Glfw[F, Monitor, Window],
                                                              skija : SkijaInit[F],
                                                              windowSize : Rect[Float]
                                                            ) : Resource[F, AtomicCell[F, SkiaRenderTarget]] =
    for
      context <- skija.createDirectContext
      scale <- Resource.eval(glfw.primaryMonitorScale)
      renderTarget <- Resource.eval(skija.createRenderTarget(context, windowSize.width, windowSize.height, scale))
      renderTargetCell <- Resource.eval(AtomicCell[F].of(renderTarget))
    yield renderTargetCell
  end createRenderTarget

  def recreateRenderTarget[F[_] : Async](
                                          skija: SkijaInit[F],
                                          cell : AtomicCell[F, SkiaRenderTarget],
                                          size : Rect[Float]
                                        ): F[Unit] =
    cell.evalUpdate(state =>
      skija.createRenderTarget(state.directContext, size.width.toFloat, size.height.toFloat, state.dpi)
    )
  end recreateRenderTarget

  def registerCallbacks[
    F[_] : Apply,
    Monitor,
    Window <: me.katze.gui4s.glfw.GlfwWindow[F, Monitor, MeasurementUnit],
    MeasurementUnit
  ](
    glfw: Glfw[F, Monitor, Window],
    window: Window,
    glfwCallbacks: GlfwCallbacks[F[Unit], MeasurementUnit]
  ): F[Unit] =
    window.windowResizeCallback(glfwCallbacks.onWindowResized)
      *> window.mouseButtonCallback(glfwCallbacks.onMouseClick)
      *> window.keyCallback(glfwCallbacks.onKeyPress)
      *> window.scrollCallback(glfwCallbacks.onScroll)
  end registerCallbacks
end SkijaSimpleDrawApi

@experimental
def skijaDrawLoop[
  F[+_] : {Console as C, ForeighFunctionInterface, Clock},
  Monitor,
  Window <: me.katze.gui4s.glfw.GlfwWindow[F, Monitor, Float],
  DownEvent,
  MeasurementUnit
](backend : SkijaBackend[F, Monitor, Window, DownEvent])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.windowShouldNotClose)(
      currentWidget.flatMap(widget =>
        backend.drawState((widget.draw |+| flush[F, Window, Monitor, Float]).run) *> backend.pollEvents
      ).timed.flatMap((duration, _) => C.println(duration))
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
