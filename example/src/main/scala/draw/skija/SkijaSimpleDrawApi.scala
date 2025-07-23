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
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.*
import org.lwjgl.opengl.GL.createCapabilities
import scalacache.caffeine.CaffeineCache

import scala.annotation.experimental

@experimental
object SkijaSimpleDrawApi:
  final case class GlfwCallbacks[F](
                                      onWindowResized: (newSize : Size) => F,
                                      onMouseClick: (Int, KeyAction, KeyModes) => F,
                                      onMouseMove: (Double, Double) => F,
                                      onKeyPress: (Int, Int, KeyAction, KeyModes) => F,
                                      onScroll : (xoffset : Double, yoffset : Double) => F
                                    )


  def createForTests[
    F[+_] : {Async, Console},
  ](
      queue : QueueSink[F, SkijaDownEvent],
      settings : WindowCreationSettings,
      ffi: ForeighFunctionInterface[F],
      callbacks : GlfwCallbacks[F[Unit]],
    ): Resource[F, SkijaBackend[F, OglGlfwWindow[F], Long]] =
    for
      skija <- Resource.eval(SkijaImpl(ffi))
      dispatcher <- Dispatcher.sequential[F]
      supervisor <- Supervisor[F]
      glfw: Glfw[F, OglGlfwWindow[F]] <- GlfwImpl[F](dispatcher)(using ffi)
      res <- createForTests(queue, glfw, ffi(createCapabilities()), skija, dispatcher, supervisor, settings, callbacks)
    yield res
  end createForTests

  def createForTests[
    F[+_] : {Async, Console}, Window <: me.katze.gui4s.glfw.GlfwWindow[F, Long]
  ](
     queue : QueueSink[F, SkijaDownEvent],
     glfw : Glfw[F, Window],
     createGlCapabilities : F[Unit],
     skija : Skija[F],
     dispatcher : Dispatcher[F],
     supervisor : Supervisor[F],
     windowSettings : WindowCreationSettings,
     callbacks : GlfwCallbacks[F[Unit]],
    ): Resource[F, SkijaBackend[F, Window, Long]] =
    for
      window <- glfw.createWindow(windowSettings)
      _ <- Resource.eval(glfw.createOGLContext(window, createGlCapabilities))
      renderTargetCell <- createRenderTarget(glfw, skija, windowSettings.size)
      _ <- Resource.eval(
        registerCallbacks(
          glfw,
          window,
          addRenderTargetRecreation(
            callbacks,
            recreateRenderTarget(skija, renderTargetCell, _)
          )
        )
      )
      shaper <- skija.createShaper
      cache <- Resource.eval(CaffeineCache[F, (String, SkijaTextStyle, Option[Pixel]), Sized[Pixel, SkijaPlacedText]])
    yield SkijaBackend(queue, glfw, window, renderTargetCell, dispatcher, supervisor, shaper, cache)
  end createForTests

  def addRenderTargetRecreation[F : Monoid](callbacks: GlfwCallbacks[F], recreation : Size => F) : GlfwCallbacks[F] =
    callbacks.copy(
      onWindowResized = newSize =>
        recreation(newSize) |+| callbacks.onWindowResized(newSize)
    )
  end addRenderTargetRecreation


  def createRenderTarget[F[_] : Concurrent, Window](
                                                      glfw : Glfw[F, Window],
                                                      skija : Skija[F],
                                                      windowSize : Size
                                                    ) : Resource[F, AtomicCell[F, SkiaRenderTarget]] =
    for
      context <- skija.createDirectContext
      scale <- Resource.eval(glfw.primaryMonitorScale)
      renderTarget <- Resource.eval(skija.createRenderTarget(context, windowSize.width, windowSize.height, scale))
      renderTargetCell <- Resource.eval(AtomicCell[F].of(renderTarget))
    yield renderTargetCell
  end createRenderTarget

  def recreateRenderTarget[F[_] : Async](
                                          skija: Skija[F],
                                          cell : AtomicCell[F, SkiaRenderTarget],
                                          size : Size
                                        ): F[Unit] =
    cell.evalUpdate(state =>
      skija.createRenderTarget(state.directContext, size.width, size.height, state.dpi)
    )
  end recreateRenderTarget

  def registerCallbacks[
    F[_] : Apply,
    Window <: me.katze.gui4s.glfw.GlfwWindow[F, Monitor],
    Monitor
  ](
    glfw: Glfw[F, Window],
    window: Window,
    glfwCallbacks: GlfwCallbacks[F[Unit]]
  ): F[Unit] =
    window.windowResizeCallback(glfwCallbacks.onWindowResized)
      *> window.mouseButtonCallback(glfwCallbacks.onMouseClick)
      *> window.keyCallback(glfwCallbacks.onKeyPress)
      *> window.scrollCallback(glfwCallbacks.onScroll)
  end registerCallbacks
end SkijaSimpleDrawApi

@experimental
def skijaDrawLoop[F[+_] : {Console as C, ForeighFunctionInterface, Clock}, Window <: me.katze.gui4s.glfw.GlfwWindow[F, Monitor], Monitor](backend : SkijaBackend[F, Window, Monitor])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.windowShouldNotClose)(
      currentWidget.flatMap(widget =>
        backend.drawState((widget.draw |+| flush[F, Window, Monitor]).run) *> backend.pollEvents
      ).timed.flatMap((duration, _) => C.println(duration))
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
