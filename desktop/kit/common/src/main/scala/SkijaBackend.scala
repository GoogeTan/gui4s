package gui4s.desktop.kit
package common

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import cats.effect.*
import cats.effect.std.{AtomicCell, Dispatcher, QueueSink}
import cats.syntax.all.*
import cats.{Apply, Functor, Monad, Monoid}
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.desktop.skija.*
import gui4s.glfw.*
import gui4s.glfw.GlfwWindow.*
import io.github.humbleui.skija.Canvas

final case class SkijaBackend[
  IO[_],
  Monitor,
  Window,
  DownEvent
](
  queue : QueueSink[IO, DownEvent],
  skija : SkijaInit[IO],
  glfw : Glfw[IO, Monitor, Window],
  window: Window,
  renderTargetCell : AtomicCell[IO, SkiaRenderTarget],
)(
  using val windowIsGlfwWindow : GlfwWindow[IO, Window, Monitor, Float]
):
  def windowBounds(using Functor[IO]) : IO[Rect[Float]] =
    window.frameBufferSize
  end windowBounds

  def mousePosition : IO[Point2d[Float]] =
    window.currentMousePosition
  end mousePosition

  def windowShouldNotClose(using M : Monad[IO]) : IO[Boolean] =
    window.shouldNotClose
  end windowShouldNotClose

  def drawFrame[T](using M : Monad[IO])(ffi : ForeignFunctionInterface[IO], f : Canvas => IO[T]) : IO[T] =
    renderTargetCell.evalModify(
      renderTarget =>
        f(renderTarget.canvas)
          .map(result => (renderTarget, result))
          <* ffi(renderTarget.directContext.flush())
          <* window.swapBuffers
          <* glfw.pollEvents
    )
  end drawFrame

  def raiseEvent(event : DownEvent) : IO[Unit] =
    queue.offer(event)
  end raiseEvent
end SkijaBackend

object SkijaBackend:
  def create[
    F[_] : Async,
    DownEvent
  ](
    queue : QueueSink[F, DownEvent],
    settings : WindowCreationSettings[Float],
    ffi: ForeignFunctionInterface[F],
    callbacks : GlfwCallbacks[F[Unit], Float],
    unsafeRunF : F[Unit] => Unit
  ): Resource[F, SkijaBackend[F, Long, OglGlfwWindow, DownEvent]] =
    for
      skija <- Resource.eval(SkijaInitImpl(ffi))
      glfw: Glfw[F, Long, OglGlfwWindow] <- GlfwImpl[F]()(using ffi)
      given GlfwWindow[F, OglGlfwWindow, Long, Float] = OglWindowIsGlfwWindow(ffi, unsafeRunF)
      res <- create(queue, glfw, skija, settings, callbacks)
    yield res
  end create

  def create[
    F[_] : Async,
    Monitor,
    Window : GlfwWindowT[F, Monitor, Float],
    DownEvent
  ](
      queue : QueueSink[F, DownEvent],
      glfw : Glfw[F, Monitor, Window],
      skija : SkijaInit[F],
      windowSettings : WindowCreationSettings[Float],
      callbacks : GlfwCallbacks[F[Unit], Float],
    ): Resource[F, SkijaBackend[F, Monitor, Window, DownEvent]] =
    for
      window <- glfw.createWindow(windowSettings)
      _ <- Resource.eval(window.makeContextCurrent)
      renderTargetCell <- createRenderTarget(glfw, skija, windowSettings.size)
      _ <- Resource.eval(
        registerCallbacks[F, Monitor, Window, Float](
          window,
          addRenderTargetRecreation[F[Unit], Float](
            callbacks,
            recreateRenderTarget(skija, renderTargetCell, _)
          )
        )
      )
    yield SkijaBackend[F, Monitor, Window, DownEvent](queue, skija, glfw, window, renderTargetCell)
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


  def createRenderTarget[F[_] : Concurrent, Monitor, Window](
                                                              glfw : Glfw[F, Monitor, Window],
                                                              skija : SkijaInit[F],
                                                              windowSize : Rect[Float]
                                                            ) : Resource[F, AtomicCell[F, SkiaRenderTarget]] =
    for
      context <- skija.createDirectContext
      scale <- Resource.eval(glfw.primaryMonitorScale)
      renderTarget <- Resource.eval(skija.createRenderTarget(context, windowSize.width * scale, windowSize.height * scale))
      renderTargetCell <- Resource.eval(AtomicCell[F].of(renderTarget))
    yield renderTargetCell
  end createRenderTarget

  def recreateRenderTarget[F[_] : Async](
                                          skija: SkijaInit[F],
                                          cell : AtomicCell[F, SkiaRenderTarget],
                                          newSize : Rect[Float]
                                        ): F[Unit] =
    cell.evalUpdate(state =>
      skija.createRenderTarget(state.directContext, newSize.width, newSize.height)
    )
  end recreateRenderTarget

  def registerCallbacks[
    IO[_] : Apply,
    Monitor,
    Window : GlfwWindowT[IO, Monitor, MeasurementUnit],
    MeasurementUnit
  ](
      window: Window,
      glfwCallbacks: GlfwCallbacks[IO[Unit], MeasurementUnit]
    ): IO[Unit] =
    window.frameBufferResizeCallback(glfwCallbacks.onWindowResized)
      *> window.mouseButtonCallback(glfwCallbacks.onMouseClick)
      *> window.keyCallback(glfwCallbacks.onKeyPress)
      *> window.scrollCallback(glfwCallbacks.onScroll)
  end registerCallbacks
end SkijaBackend

