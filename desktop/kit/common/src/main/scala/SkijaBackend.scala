package gui4s.desktop.kit
package common

import catnip.syntax.all.given
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
  Resource[_],
  CallbackIO[_],
  Monitor,
  Window,
  DownEvent
](
  queue : QueueSink[CallbackIO, DownEvent],
  skija : SkijaInit[CallbackIO, Resource],
  glfw : PostInit[IO, Resource, CallbackIO[Unit], Monitor, Window],
  window: Window,
  renderTargetCell : ResourceCell[CallbackIO, Resource, SkiaRenderTarget],
):
  def windowBounds(using Functor[IO]) : IO[Rect[Float]] =
    glfw.getWindowSize(window).map(Rect(_, _))
  end windowBounds

  def mousePosition : IO[Point2d[Float]] =
    ???
  end mousePosition

  def windowShouldNotClose(using Functor[IO]) : IO[Boolean] =
    glfw.shouldNotWindowClose(window)
  end windowShouldNotClose

  def drawFrame[T](using Sync[CallbackIO])(f : Canvas => CallbackIO[T]) : CallbackIO[T] =
    renderTargetCell.eval(
      renderTarget =>
        f(renderTarget.canvas)
          <* flush(renderTarget.directContext)
          <* glfw.swapBuffers(window)
          <* glfw.pollEvents()
    )
  end drawFrame

  def raiseEvent(event : DownEvent) : CallbackIO[Unit] =
    queue.offer(event)
  end raiseEvent
end SkijaBackend

object SkijaBackend:
  def create[
    IO[_] : Async,
    Resource[_] : Monad,
    CallbackIO[_] : Concurrent,
    Monitor,
    Window,
    DownEvent
  ](
     queue : QueueSink[CallbackIO, DownEvent],
     glfw : PostInit[IO, Resource, CallbackIO[Unit], Monitor, Window],
     windowSettings : WindowCreationSettings[Monitor, Window],
     callbacks : GlfwCallbacks[CallbackIO[Unit], Float],
     eval : IO ~> Resource,
     fromAutoCloseable : [T <: AutoCloseable] => CallbackIO[T] => Resource[T],
     liftIO: CallbackIO ~> IO
    ): Resource[SkijaBackend[IO, Resource, CallbackIO, Monitor, Window, DownEvent]] =
    val skija = SkijaInitImpl[CallbackIO, Resource](liftIO.andThen(eval), fromAutoCloseable)
    for
      window <- glfw.createWindow(windowSettings)
      _ <- eval(glfw.makeContextCurrent(window))
      canvasSize <- glfw.getFramebufferSize(window)
      renderTargetCell <- createRenderTarget[CallbackIO, Resource, Monitor, Window](skija, Rect(canvasSize._1, canvasSize._2), liftIO.andThen(eval))
      _ <- eval(
        registerCallbacks[IO, Resource, CallbackIO[Unit], Monitor, Window](
          glfw,
          window,
          addRenderTargetRecreation[CallbackIO[Unit], Float](
            callbacks,
            recreateRenderTarget[CallbackIO, Resource](skija, renderTargetCell, _)
          )
        )
      )
    yield SkijaBackend[IO, Resource, Monitor, Window, DownEvent](queue, skija, glfw, window, renderTargetCell)
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
    Resource[_] : Monad,
    Monitor,
    Window
  ](
     skija : SkijaInit[IO, Resource],
     canvasSize : Rect[Float],
     eval : IO ~> Resource
    ) : Resource[ResourceCell[IO, Resource, SkiaRenderTarget]] =
    for
      context <- skija.createDirectContext
      renderTargetCell <- ResourceCell.atomic(
          skija.createRenderTarget(
            context = context,
            width = canvasSize.width,
            height = canvasSize.height
          ),
          ???,
          ???
        )
    yield renderTargetCell
  end createRenderTarget

  def recreateRenderTarget[IO[_], Resource[_]](
                                                skija: SkijaInit[IO, Resource],
                                                cell : ResourceCell[IO, Resource, SkiaRenderTarget],
                                                newSize : Rect[Float]
                                              ): IO[Unit] =
    cell.evalUpdate(state =>
      skija.createRenderTarget(state.directContext, newSize.width, newSize.height)
    )
  end recreateRenderTarget

  def registerCallbacks[
    IO[_] : Apply,
    Resource[_],
    CallbackIO,
    Monitor,
    Window,
  ](
      glfw : PostInit[IO, Resource, CallbackIO, Monitor, Window],
      window: Window,
      glfwCallbacks: GlfwCallbacks[CallbackIO, Float]
    ): IO[Unit] =
    glfw.setFramebufferSizeCallback(window, (window, width, height) => glfwCallbacks.onWindowResized(Rect(width, height)))
      *> glfw.setMouseButtonCallback(window, (window, button, action, modes) => glfwCallbacks.onMouseClick(button, action, modes))
      *> glfw.setKeyCallback(window, (window, key, code, action, modes) => glfwCallbacks.onKeyPress(key, code, action, modes))
      *> glfw.setScrollCallback(window, (window, xoffset, yoffset) => glfwCallbacks.onScroll(xoffset.toFloat, yoffset.toFloat))
  end registerCallbacks
end SkijaBackend

