package me.katze.gui4s.example
package draw.skija

import cats.{Functor, Monad}
import cats.effect.std.{AtomicCell, Dispatcher, QueueSink, Supervisor}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.{Glfw, GlfwWindow}
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkiaRenderTarget, SkijaDrawState, SkijaPlacedText, SkijaTextStyle, given}
import cats.syntax.all.*
import me.katze.gui4s.layout.Sized
import scalacache.Cache

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
    
  
end SkijaBackend
