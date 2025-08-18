package me.katze.gui4s.example
package skija

import loop.{DrawLoop, drawLoop}

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.effect.*
import cats.effect.std.{AtomicCell, Console, Dispatcher, QueueSink}
import cats.syntax.all.*
import cats.{Apply, Functor, Monad, MonadError, Monoid}
import io.github.humbleui.skija.Canvas
import me.katze.gui4s.geometry.{Point2d, Rect}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.glfw.GlfwWindow.*
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.draw.Drawable

final case class SkijaBackend[
  F[_],
  Monitor,
  Window,
  DownEvent
](
  queue : QueueSink[F, DownEvent],
  skija : SkijaInit[F],
  glfw : Glfw[F, Monitor, Window],
  window: Window,
  renderTargetCell : AtomicCell[F, SkiaRenderTarget],
)(
  using val windowIsGlfwWindow : GlfwWindow[F, Window, Monitor, Float]
):
  def windowBounds(using Functor[F]) : F[Rect[Float]] =
    window.frameBufferSize
  end windowBounds

  def mousePosition : F[Point2d[Float]] =
    window.currentMousePosition
  end mousePosition

  def windowShouldNotClose(using M : Monad[F]) : F[Boolean] =
    window.shouldNotClose
  end windowShouldNotClose

  def drawFrame[T](using M : Monad[F])(ffi : ForeighFunctionInterface[F], f : Canvas => F[T]) : F[T] =
    renderTargetCell.evalModify(
      renderTarget =>
        f(renderTarget.canvas)
          .map(result => (renderTarget, result))
          <* ffi(renderTarget.directContext.flush())
          <* window.swapBuffers
          <* glfw.pollEvents
    )
  end drawFrame

  def raiseEvent(event : DownEvent) : F[Unit] =
    queue.offer(event)
  end raiseEvent
end SkijaBackend

object SkijaBackend:
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
    ): Resource[F, SkijaBackend[F, Long, OglGlfwWindow, DownEvent]] =
    for
      skija <- Resource.eval(SkijaInitImpl(ffi))
      dispatcher <- Dispatcher.sequential[F]
      glfw: Glfw[F, Long, OglGlfwWindow] <- GlfwImpl[F](dispatcher)(using ffi)
      given GlfwWindow[F, OglGlfwWindow, Long, Float] = OglWindowIsGlfwWindow(ffi, dispatcher.unsafeRunAndForget)
      res <- createForTests(queue, glfw, skija, settings, callbacks)
    yield res
  end createForTestsTrue

  def createForTests[
    F[+_] : {Async, Console},
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

def skijaDrawLoop[
  F[+_] : Console,
  Draw,
  Widget
](
    widgetIsDrawable : Drawable[Widget, Draw],
    shouldContinue : F[Boolean],
    drawFrame : Draw => F[Unit]
)(using MonadError[F, Throwable]) : DrawLoop[F, Widget] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, shouldContinue)(
      currentWidget.flatMap(widget => drawFrame(widgetIsDrawable(widget)))
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop

def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
  c.println[String]("Error in draw loop: " + exception.toString).map(_ => Some(ExitCode.Error))
end drawLoopExceptionHandler
