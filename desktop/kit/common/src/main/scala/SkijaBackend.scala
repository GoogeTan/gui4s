package gui4s.desktop.kit
package common

import catnip.syntax.all.given
import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.effect.*
import cats.effect.std.{AtomicCell, Dispatcher, QueueSink}
import cats.syntax.all.*
import cats.{Applicative, Apply, Functor, Monad, Monoid, ~>}
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.desktop.skija.*
import gui4s.desktop.skija.DirectContext.flush
import glfw4s.core.*
import glfw4s.core.pure.*
import io.github.humbleui.skija.Canvas

final case class SkijaBackend[
  IO[_],
  Monitor,
  Window,
  DownEvent
](
  queue : QueueSink[IO, DownEvent],
  skija : SkijaInit[IO],
  glfw : PostInit[IO, IO[Unit], Monitor, Window],
  window: Window,
  renderTargetCell : ResourceCell[IO, SkiaRenderTarget],
):
  def windowBounds(using Functor[IO]) : IO[Rect[Float]] =
    glfw.getWindowSize(window).map(Rect(_, _))
  end windowBounds

  def mousePosition(using Functor[IO]) : IO[Point2d[Float]] =
    glfw.glfwGetCursorPos(window).map(Point2d(_, _))
  end mousePosition

  def windowShouldNotClose(using Functor[IO]) : IO[Boolean] =
    glfw.shouldNotWindowClose(window)
  end windowShouldNotClose

  def drawFrame[T](using Sync[IO])(f : Canvas => IO[T]) : IO[T] =
    renderTargetCell.eval(
      renderTarget =>
        f(renderTarget.canvas)
          <* flush(renderTarget.directContext)
          <* glfw.swapBuffers(window)
          <* glfw.pollEvents(),
      FunctionK.id
    )
  end drawFrame

  def drawFrame[T](using Sync[IO])(f : ReaderT[IO, Canvas, T]) : IO[T] =
    drawFrame(f.run)
  end drawFrame

  def raiseEvent(event : DownEvent) : IO[Unit] =
    queue.offer(event)
  end raiseEvent
end SkijaBackend

object SkijaBackend:
  def create[
    IO[_] : Async,
    Monitor,
    Window,
    DownEvent
  ](
     queue : QueueSink[IO, DownEvent],
     glfw : PostInit[IO, IO[Unit], Monitor, Window],
     windowSettings : WindowCreationSettings[Monitor, Window],
     callbacks : GlfwCallbacks[IO[Unit], Float],
    ): Resource[IO, SkijaBackend[IO, Monitor, Window, DownEvent]] =
    val skija = SkijaInitImpl[IO]
    for
      window <- glfw.createWindow(windowSettings)
      _ <- Resource.eval(glfw.makeContextCurrent(window))
      canvasSize <- Resource.eval(glfw.getFramebufferSize(window))
      renderTargetCell <- createRenderTarget[IO, Monitor, Window](skija, Rect(canvasSize._1, canvasSize._2))
      _ <- Resource.eval(
        registerCallbacks[IO, IO[Unit], Monitor, Window](
          glfw,
          window,
          addRenderTargetRecreation[IO[Unit], Float](
            callbacks,
            recreateRenderTarget[IO](skija, renderTargetCell, _)
          )
        )
      )
    yield SkijaBackend[IO, Monitor, Window, DownEvent](queue, skija, glfw, window, renderTargetCell)
  end create

  def addRenderTargetRecreation[F : Monoid, MeasurementUnit](
                                                              callbacks: GlfwCallbacks[F, MeasurementUnit],
                                                              recreation : Rect[MeasurementUnit] => F
                                                            ) : GlfwCallbacks[F, MeasurementUnit] =
    callbacks.copy(
      onWindowResized = newSize =>
        recreation(newSize) |+| callbacks.onWindowResized(newSize)
    )
  end addRenderTargetRecreation


  def createRenderTarget[
    IO[_] : Concurrent,
    Monitor,
    Window
  ](
     skija : SkijaInit[IO],
     canvasSize : Rect[Float],
    ) : Resource[IO, ResourceCell[IO, SkiaRenderTarget]] =
    for
      context <- skija.createDirectContext
      renderTargetCell <- ResourceCell.atomic(
          skija.createRenderTarget(
            context = context,
            width = canvasSize.width,
            height = canvasSize.height
          ),
        )
    yield renderTargetCell
  end createRenderTarget

  def recreateRenderTarget[IO[_]](
                                    skija: SkijaInit[IO],
                                    cell : ResourceCell[IO, SkiaRenderTarget],
                                    newSize : Rect[Float]
                                  ): IO[Unit] =
    cell.evalUpdate(state =>
      skija.createRenderTarget(state.directContext, newSize.width, newSize.height)
    )
  end recreateRenderTarget

  def registerCallbacks[
    IO[_] : Apply,
    CallbackResult,
    Monitor,
    Window,
  ](
     glfw : PostInit[IO, CallbackResult, Monitor, Window],
     window: Window,
     glfwCallbacks: GlfwCallbacks[CallbackResult, Float]
    ): IO[Unit] =
    glfw.setFramebufferSizeCallback(window, (window, width, height) => glfwCallbacks.onWindowResized(Rect(width, height)))
      *> glfw.setMouseButtonCallback(window, (window, button, action, modes) => glfwCallbacks.onMouseClick(button, action, modes))
      *> glfw.setKeyCallback(window, (window, key, code, action, modes) => glfwCallbacks.onKeyPress(key, code, action, modes))
      *> glfw.setScrollCallback(window, (window, xoffset, yoffset) => glfwCallbacks.onScroll(xoffset.toFloat, yoffset.toFloat))
  end registerCallbacks
end SkijaBackend

