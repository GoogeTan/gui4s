package me.katze.gui4s.example
package draw.skija

import draw.{Drawable, drawLoopExceptionHandler}

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.effect.std.{AtomicCell, Console, Dispatcher}
import cats.effect.{Async, Clock, Concurrent, ExitCode, Resource}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.{Apply, MonadError, Monoid}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.*
import org.lwjgl.opengl.GL.createCapabilities
import scalacache.caffeine.CaffeineCache

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
      settings : WindowCreationSettings,
      ffi: FFI[F],
      callbacks : GlfwCallbacks[F[Unit]],
    ): Resource[F, SkijaBackend[F, OglWindow]] =
    for
      skija <- Resource.eval(SkijaImpl(ffi))
      dispatcher <- Dispatcher.sequential[F]
      glfw: Glfw[F, OglWindow] <- GlfwImpl[F](dispatcher)(using ffi)
      res <- createForTests(glfw, ffi(createCapabilities()), skija, dispatcher, settings, callbacks)
    yield res
  end createForTests

  def createForTests[
    F[+_] : {Async, Console}, Window
  ](
      glfw : Glfw[F, Window],
      createGlCapabilities : F[Unit],
      skija : Skija[F],
      dispatcher : Dispatcher[F],
      windowSettings : WindowCreationSettings,
      callbacks : GlfwCallbacks[F[Unit]],
    ): Resource[F, SkijaBackend[F, Window]] =
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
      cache <- Resource.eval(CaffeineCache[F, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]])
    yield SkijaBackend(glfw, window, renderTargetCell, dispatcher, shaper, cache)
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
      scale <- Resource.eval(glfw.primaryMonitorScale)
      context <- skija.createDirectContext
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

  def registerCallbacks[F[_] : Apply, Window](
                                                glfw: Glfw[F, Window],
                                                window: Window,
                                                glfwCallbacks: GlfwCallbacks[F[Unit]]
                                              ): F[Unit] =
    glfw.windowResizeCallback(window, glfwCallbacks.onWindowResized)
      *> glfw.mouseButtonCallback(window, glfwCallbacks.onMouseClick)
      //*> glfw.cursorPosCallback(window, glfwCallbacks.onMouseMove)
      *> glfw.keyCallback(window, glfwCallbacks.onKeyPress)
      *> glfw.scrollCallback(window, glfwCallbacks.onScroll)
  end registerCallbacks
end SkijaSimpleDrawApi

def skijaDrawLoop[F[+_] : {Console as C, FFI, Clock}, Window](backend : SkijaBackend[F, Window])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.windowShouldNotClose)(
      currentWidget.flatMap(widget =>
        backend.drawState((widget.draw |+| flush[F, Window]).run) *> backend.pollEvents
      )//.timed.flatMap((duration, _) => C.println(duration))
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
