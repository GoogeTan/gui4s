package me.katze.gui4s.example
package draw.skija

import cats.{Functor, Monad}
import cats.effect.std.{AtomicCell, Dispatcher}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.Glfw
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{Pixel, SkiaRenderTarget, SkijaDrawState, SkijaPlacedText, SkijaTextStyle, given}
import cats.syntax.all.*
import me.katze.gui4s.layout.Sized
import scalacache.Cache

final case class SkijaBackend[
  F[_],
  Window <: me.katze.gui4s.glfw.Window[F, Monitor],
  Monitor
](
  glfw : Glfw[F, Window],
  window: Window,
  private val renderTargetCell : AtomicCell[F, SkiaRenderTarget],
  globalDispatcher : Dispatcher[F],
  globalShaper : Shaper,
  globalTextCache : Cache[F, (String, SkijaTextStyle, Option[Pixel]), Sized[Pixel, SkijaPlacedText]]
):
  def windowBounds(using Functor[F]) : F[Bounds[Pixel]] =
    window.frameBufferSize.map(a => new Bounds(Pixel(a.width.toFloat), Pixel(a.height.toFloat)))
  end windowBounds

  def windowShouldNotClose(using M : Monad[F]) : F[Boolean] =
    window.shouldNotClose
  end windowShouldNotClose

  def drawState[T](using M : Monad[F])(f : SkijaDrawState[F, Window] => F[T]) : F[T] =
    renderTargetCell.evalModify(
      renderTarget =>
        f(SkijaDrawState(renderTarget.directContext, glfw, window, renderTarget.canvas))
          .map(result => (renderTarget, result))
    )
  end drawState

  def pollEvents: F[Unit] =
    glfw.pollEvents
  end pollEvents
end SkijaBackend
