package me.katze.gui4s.example
package draw.skija

import api.impl.LayoutPlacementMeta

import io.github.humbleui.skija.TextBlob
import me.katze.gui4s.widget.library.{LayoutDraw, TextDraw}
import api.impl.DrawMonad
import draw.{Drawable, SimpleDrawApi, TextStyle, drawLoopExceptionHandler}
import impl.{*, given}

import cats.data.ReaderT
import cats.effect.Async
import cats.effect.kernel.Resource
import cats.effect.std.{Console, Dispatcher}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad, MonadError, Monoid, effect}
import io.github.humbleui.skija.examples.Skia
import io.github.humbleui.skija.examples.Skia.SkiaRenderTarget
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, DirectContext, Font, Paint, Typeface}
import io.github.humbleui.types.Rect
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.TestFuncs

final case class SkijaDrawState[F[_], Window](context : DirectContext, glfw: Glfw[F, Window], window : Window, canvas : Canvas)

type SkijaDraw[F[_], Window] = ReaderT[F, SkijaDrawState[F, Window], Unit]

given [F[_] : {Impure, Monad}, Window]: DrawMonad[SkijaDraw[F, Window], Float] with
  override def move(dx: Float, dy: Float, effect: SkijaDraw[F, Window]): SkijaDraw[F, Window] =
    ReaderT[F, SkijaDrawState[F, Window], Unit].apply(
      state =>
        TestFuncs[F].moveAndBack(state.canvas, dx, dy, effect.run(state))
    )
end given

given[F[_] : Applicative, W]: Monoid[SkijaDraw[F, W]] with
  override def empty: SkijaDraw[F, W] = ReaderT.pure[F, SkijaDrawState[F, W], Unit](())

  override def combine(x: SkijaDraw[F, W], y: SkijaDraw[F, W]): SkijaDraw[F, W] =
    ReaderT[F, SkijaDrawState[F, W], Unit]:
      state => 
        x(state) *> y(state)
  end combine
end given

given skijaLayoutDraw[F[_] : {Impure, Monad}, Window]: LayoutDraw[SkijaDraw[F, Window], LayoutPlacementMeta[Float]] =
  layoutDrawImpl[SkijaDraw[F, Window], Float]

given skijaTextDraw[F[_] : Impure as I, Window, MeasurementUnit]: TextDraw[SkijaDraw[F, Window], TextBlob] =
  (text, meta) =>
    ReaderT[F, SkijaDrawState[F, Window], Unit](
      state =>
        I:
          state.canvas.drawTextBlob(meta, 0, 0, new Paint())
    )
end skijaTextDraw

def flush[F[_] : {Monad, Impure as I}, Window]: ReaderT[F, SkijaDrawState[F, Window], Unit] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    I(state.context.flush()) *> state.glfw.swapBuffers(state.window)
  )
end flush

final case class SkijaBackend[F[_]](
                                      glfw : Glfw[F, OglWindow],
                                      window: OglWindow,
                                      renderTarget : SkiaRenderTarget
                                    ):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    glfw.windowSize(window).map(a => new Bounds(a.width, a.height))
  end windowBounds
end SkijaBackend

object SkijaSimpleDrawApi:
  def createForTests[F[+_] : {Impure as I, Async}] : Resource[F, SkijaBackend[F]] =
    for
      dispatcher <- Dispatcher.sequential[F]
      glfw = GlfwImpl[F]([T] => (value : F[T]) => dispatcher.unsafeRunSync(value))
      dpi <- effect.Resource.eval(glfw.monitorScale(0))
      rt <- Skia.initSkia(500, 500, dpi._1)
      window <- glfw.createWindow("Test", Size(500, 500), true, true, true)
    yield SkijaBackend(glfw, window, rt)
  end createForTests
end SkijaSimpleDrawApi


def skijaDrawLoop[F[+_] : {Console, Impure}, Window](using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler)(
      currentWidget.map(widget =>
        widget.draw |+| flush[F, Window]
      )
    )
end skijaDrawLoop
