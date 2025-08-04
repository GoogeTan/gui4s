package me.katze.gui4s.example
package skija

import catnip.ForeighFunctionInterface
import cats.{Apply, MonadError, Monoid}
import cats.effect.{Async, Clock, Concurrent, ExitCode, Resource}
import cats.effect.std.Console
import me.katze.gui4s.geometry.{Point2d, Rect}
import me.katze.gui4s.glfw.{GlfwImpl, KeyAction, KeyModes, OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.skija.{SkijaDraw, SkijaInit, SkijaInitImpl, flush}
import me.katze.gui4s.widget.draw.Drawable
import scalacache.caffeine.CaffeineCache

import cats.{Functor, Monad}
import cats.effect.std.{AtomicCell, Dispatcher, QueueSink, Supervisor}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.{Glfw, GlfwWindow}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkiaRenderTarget, SkijaDrawState, SkijaPlacedText, SkijaTextStyle, given}
import cats.syntax.all.*
import me.katze.gui4s.layout.Sized
import scalacache.Cache

import catnip.syntax.all.{*, given}

import scala.annotation.experimental

@experimental
final case class SkijaBackend[
  F[_],
  Monitor,
  +Window <: GlfwWindow[F, Monitor, Float],
  DownEvent
](
  queue : QueueSink[F, DownEvent],
  glfw : Glfw[F, Monitor, ?],
  window: Window,
  renderTargetCell : AtomicCell[F, SkiaRenderTarget],
  globalDispatcher : Dispatcher[F],
  globalSupervisor : Supervisor[F],
  globalShaper : Shaper,
  globalTextCache : Cache[F, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    window.frameBufferSize.map(a => new Bounds(a.width.toFloat, a.height.toFloat))
  end windowBounds

  def windowShouldNotClose(using M : Monad[F]) : F[Boolean] =
    window.shouldNotClose
  end windowShouldNotClose

  def drawState[Window1 >: Window, T](using M : Monad[F])(f : SkijaDrawState[F, Window1] => F[T]) : F[T] =
    renderTargetCell.evalModify(
      renderTarget =>
        f(SkijaDrawState(renderTarget.directContext, window, renderTarget.canvas))
          .map(result => (renderTarget, result))
    )
  end drawState

  def pollEvents: F[Unit] =
    glfw.pollEvents
  end pollEvents

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
    ): Resource[F, SkijaBackend[F, Long, GlfwWindow[F, Long, Float], DownEvent]] =
    for
      skija <- Resource.eval(SkijaInitImpl(ffi))
      dispatcher <- Dispatcher.sequential[F]
      supervisor <- Supervisor[F]
      glfw: Glfw[F, Long, OglGlfwWindow[F]] <- GlfwImpl[F](dispatcher)(using ffi)
      res <- createForTests(queue, glfw, skija, dispatcher, supervisor, settings, callbacks)
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
      skija : SkijaInit[F],
      dispatcher : Dispatcher[F],
      supervisor : Supervisor[F],
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
    IO[_] : Apply,
    Monitor,
    Window <: me.katze.gui4s.glfw.GlfwWindow[IO, Monitor, MeasurementUnit],
    MeasurementUnit
  ](
      window: Window,
      glfwCallbacks: GlfwCallbacks[IO[Unit], MeasurementUnit]
    ): IO[Unit] =
    window.windowResizeCallback(glfwCallbacks.onWindowResized)
      *> window.mouseButtonCallback(glfwCallbacks.onMouseClick)
      *> window.keyCallback(glfwCallbacks.onKeyPress)
      *> window.scrollCallback(glfwCallbacks.onScroll)
  end registerCallbacks
end SkijaBackend

@experimental
def skijaDrawLoop[
  F[+_] : {Console as C, ForeighFunctionInterface, Clock},
  Monitor,
  Window <: me.katze.gui4s.glfw.GlfwWindow[F, Monitor, Float],
  DownEvent,
  Widget
](
    backend : SkijaBackend[F, Monitor, Window, DownEvent],
    widgetIsDrawable : Drawable[Widget, SkijaDraw[F, Window]]
)(using MonadError[F, Throwable]) : DrawLoop[F, Widget] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.windowShouldNotClose)(
      currentWidget.flatMap(widget =>
        backend.drawState((widgetIsDrawable(widget) |+| flush[F, Window, Monitor, Float]).run) *> backend.pollEvents
      ).timed.flatMap((duration, _) => C.println(duration)) // TODO Remove me
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop

// TODO Почему-то ругается на эни в интерполяции строки...
@SuppressWarnings(Array("org.wartremover.warts.Any"))
def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
  c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
end drawLoopExceptionHandler
