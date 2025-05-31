package me.katze.gui4s.example
package draw.skija

import draw.{Drawable, drawLoopExceptionHandler}

import catnip.syntax.all.given
import cats.MonadError
import cats.effect.std.{AtomicCell, Console, Dispatcher}
import cats.effect.{Async, ExitCode, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.FFI
import me.katze.gui4s.skija.*
import org.lwjgl.opengl.GL.createCapabilities

object SkijaSimpleDrawApi:
  def createForTests[F[+_] : {Async, Console}](
                                                windowSize : Size,
                                                windowTitle : String,
                                                GlfwImpure: FFI[F],
                                                CommonImpure: FFI[F],
                                                onWindowResized: (newSize : Size) => F[Unit],
                                                onMouseClick: (Int, KeyAction, KeyModes) => F[Unit],
                                                onMouseMove: (Double, Double) => F[Unit],
                                                onKeyPress: (Int, Int, KeyAction, KeyModes) => F[Unit]
                                              ): Resource[F, SkijaBackend[F, OglWindow]] =
    for
      skija <- Resource.eval(SkijaImpl(GlfwImpure))
      dispatcher <- Dispatcher.sequential[F]
      glfw: Glfw[F, OglWindow] <- GlfwImpl[F](dispatcher)(using GlfwImpure)
      _ <- glfw.createPrintErrorCallback
      window <- glfw.createWindow(
        windowTitle,
        windowSize,
        visible = true,
        resizeable = true,
        debugContext = true
      )
      _ <- Resource.eval(glfw.createOGLContext(window, GlfwImpure(createCapabilities())))
      scale <- Resource.eval(glfw.primaryMonitorScale)
      context <- skija.createDirectContext
      rt <- Resource.eval(skija.createRenderTarget(context, windowSize.width, windowSize.height, scale))
      rtCell <- Resource.eval(AtomicCell[F].of(rt))
      _ <- Resource.eval(registerCallbacks(
        glfw,
        window,
        rtCell,
        newSize => recreateRenderTarget(skija, rtCell, newSize) *> onWindowResized(newSize),
        onMouseClick,
        onMouseMove,
        onKeyPress
      )(using CommonImpure))
      shaper <- Resource.fromAutoCloseable(CommonImpure(Shaper.make()))
    yield SkijaBackend(glfw, window, rtCell, dispatcher, shaper)
  end createForTests

  def recreateRenderTarget[F[_] : Async](
                                          skija: Skija[F],
                                          cell : AtomicCell[F, SkiaRenderTarget],
                                          size : Size
                                        ): F[Unit] =
    cell.evalUpdate(state =>
      skija.createRenderTarget(state.directContext, size.width, size.height, state.dpi)
    )
  end recreateRenderTarget

  def registerCallbacks[F[_] : {FFI as I, Async, Console}](
                                                            glfw: Glfw[F, OglWindow],
                                                            window: OglWindow,
                                                            rt: AtomicCell[F, SkiaRenderTarget],
                                                            onWindowResized: (newSize : Size) => F[Unit],
                                                            onMouseClick: (Int, KeyAction, KeyModes) => F[Unit],
                                                            onMouseMove: (Double, Double) => F[Unit],
                                                            onKeyPress: (Int, Int, KeyAction, KeyModes) => F[Unit]
                                                          ): F[Unit] =
    glfw.windowResizeCallback(window, onWindowResized)
      *> glfw.mouseButtonCallback(window, onMouseClick)
      *> glfw.cursorPosCallback(window, onMouseMove)
      *> glfw.keyCallback(window, onKeyPress)
  end registerCallbacks
end SkijaSimpleDrawApi

def skijaDrawLoop[F[+_] : {Console, FFI}, Window](backend : SkijaBackend[F, Window])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.windowShouldNotClose)(
      currentWidget.flatMap(widget =>
        backend.drawState((widget.draw |+| flush[F, Window]).run) *> backend.pollEvents
      )
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
