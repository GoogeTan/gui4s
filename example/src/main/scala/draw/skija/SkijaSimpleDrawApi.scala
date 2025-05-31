package me.katze.gui4s.example
package draw.skija

import draw.{Drawable, drawLoopExceptionHandler}

import catnip.{Bi, BiMonad, FFI, FailsWith}
import catnip.cats.effect.*
import catnip.syntax.all.{*, given}
import cats.MonadError
import cats.effect.std.{AtomicCell, Console, Dispatcher}
import cats.effect.{Async, ExitCode, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.*
import me.katze.gui4s.skija.*
import org.lwjgl.opengl.GL.createCapabilities

object SkijaSimpleDrawApi:
  def createForTests[
    F[+_, +_] : {BiMonad, BiAsync, BiConcurrent, FailsWith, Bi[Console]},
    Error,
    GlfwError,
  ](
      windowSize : Size,
      windowTitle : String,
      GlfwImpure: FFI[F[GlfwError, *]],
      onWindowResized: (newSize : Size) => F[GlfwError, Unit],
      onMouseClick: (Int, KeyAction, KeyModes) => F[GlfwError, Unit],
      onMouseMove: (Double, Double) => F[GlfwError, Unit],
      onKeyPress: (Int, Int, KeyAction, KeyModes) => F[GlfwError, Unit],
      glfwAsError : (GlfwError, String) => Error
    ): Resource[F[Error, *], SkijaBackend[F[GlfwError, *], OglWindow]] =
    extension[T](value : Resource[F[GlfwError, *], T])
      def addaptGlfwError(text : String) : Resource[F[Error, *], T] =
        value.allocated
          .mapError(glfwAsError(_, "Error while allocating error"))
          

    for
      skija <- Resource.eval(SkijaImpl(GlfwImpure))
        .addaptGlfwError("Error while creating skija implementation")
      dispatcher <- Dispatcher.sequential[F[GlfwError, *]]
        .addaptGlfwError("Error while creating dispatcher")
      glfw: Glfw[F[GlfwError, *], OglWindow] <- GlfwImpl[F[GlfwError, *]](dispatcher)(using GlfwImpure)
        .addaptGlfwError("Error while creating skija implementation")
      _ <- glfw.createPrintErrorCallback
        .addaptGlfwError("Error while creating error callback")
      window <- glfw.createWindow(
        title = windowTitle,
        size = windowSize,
        visible = true,
        resizeable = true,
        debugContext = true
      )
        .addaptGlfwError("Error while creating window")
      _ <- Resource.eval(glfw.createOGLContext(window, GlfwImpure(createCapabilities())))
        .addaptGlfwError("Error while creating ogl context for window " + window.toString)
      scale <- Resource.eval(glfw.primaryMonitorScale)
        .addaptGlfwError("Error while getting primary monitor scale")
      context <- skija.createDirectContext
        .addaptGlfwError("Error while creating direct context")
      rt <- Resource.eval(skija.createRenderTarget(context, windowSize.width, windowSize.height, scale))
        .addaptGlfwError("Error while creating render target")
      rtCell <- Resource.eval(AtomicCell[F[GlfwError, *]].of(rt))
        .addaptGlfwError("Error while creating AtomicCell for render target")
      _ <- Resource.eval(registerCallbacks(
        glfw,
        window,
        rtCell,
        newSize => recreateRenderTarget(skija, rtCell, newSize) *> onWindowResized(newSize),
        onMouseClick,
        onMouseMove,
        onKeyPress
      )(using GlfwImpure))
        .addaptGlfwError("Error while registering callbacks")
      shaper <- Resource.fromAutoCloseable(GlfwImpure(Shaper.make()))
        .addaptGlfwError("Error while creating shaper")
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
