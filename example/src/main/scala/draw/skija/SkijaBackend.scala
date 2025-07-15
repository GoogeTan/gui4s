package me.katze.gui4s.example
package draw.skija

import cats.{Functor, Monad}
import cats.effect.std.{AtomicCell, Dispatcher}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.Glfw
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkiaRenderTarget, SkijaDrawState, SkijaPlacedText, SkijaTextStyle}
import cats.syntax.all.*
import me.katze.gui4s.layout.Sized
import scalacache.Cache

final case class SkijaBackend[F[_], Window](
                                              glfw : Glfw[F, Window],
                                              window: Window,
                                              private val renderTargetCell : AtomicCell[F, SkiaRenderTarget],
                                              globalDispatcher : Dispatcher[F],
                                              globalShaper : Shaper,
                                              globalTextCache : Cache[F, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
                                            ):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    glfw.frameBufferSize(window).map(a => new Bounds(a.width, a.height))
  end windowBounds

  def windowShouldNotClose(using M : Monad[F]) : F[Boolean] =
    glfw.shouldNotClose(window)
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
