package me.katze.gui4s.example
package draw.skija

import draw.{SimpleDrawApi, TextStyle}

import cats.{Applicative, Monad, Monoid, effect}
import cats.data.ReaderT
import cats.effect.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import io.github.humbleui.skija.examples.Skia
import io.github.humbleui.skija.examples.Skia.SkiaRenderTarget
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, DirectContext, Font, Paint, Typeface}
import io.github.humbleui.types.Rect
import me.katze.gui4s.example.api.impl.DrawMonad
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.skija.TestFuncs
import cats.syntax.all.*

final case class SkijaDrawState[+Window](context : DirectContext, window : Window, canvas : Canvas, shaper: Shaper)

type SkijaDraw[F[_], +W] = ReaderT[F, SkijaDrawState[W], Unit]

given [F[_] : {Impure, Monad}, Window]: DrawMonad[SkijaDraw[F, Window], Float] with
  override def move[T](dx: Float, dy: Float, effect: SkijaDraw[F, Window]): SkijaDraw[F, Window] =
    ReaderT[F, SkijaDrawState[Window], Unit].apply(
      state =>
        TestFuncs[F].moveAndBack(state.canvas, dx, dy, effect.run(state))
    )
end given

given[F[_] : Applicative, W]: Monoid[SkijaDraw[F, W]] with
  override def empty: SkijaDraw[F, W] = ReaderT.pure(())

  override def combine(x: SkijaDraw[F, W], y: SkijaDraw[F, W]): SkijaDraw[F, W] =
    ReaderT[F, SkijaDrawState[W], Unit]:
      state => 
        x(state) *> y(state)
  end combine
end given

final case class SkijaSimpleDrawApi[F[_] : {Impure as I, Monad as M}, W](G : Glfw[F] { type Window  = W })
              extends SimpleDrawApi[Float, ReaderT[F, SkijaDrawState[W], Unit]]:
  override def beginDraw: ReaderT[F, SkijaDrawState[W], Unit] =
    ReaderT[F, SkijaDrawState[G.Window], Unit](_ => M.pure(()))
  end beginDraw

  override def endDraw: ReaderT[F, SkijaDrawState[W], Unit] =
    ReaderT[F, SkijaDrawState[G.Window], Unit](state =>
      I(state.context.flush()) *> G.swapBuffers(state.window)
    )
  end endDraw

  override def text(x: Float, y: Float, text: String, style: TextStyle): ReaderT[F, SkijaDrawState[W], Unit] =
    ReaderT[F, SkijaDrawState[G.Window], Unit](state =>
      I:
        val font = new Font(Typeface.makeDefault())
        val blob = state.shaper.shape(text, font.setSize(style.size))
        state.canvas.drawTextBlob(blob, x, y, new Paint().setColor(style.color))
        ()
    )
  end text

  override def rectangle(x: Float, y: Float, width: Float, height: Float, color: Int): ReaderT[F, SkijaDrawState[W], Unit] =
    ReaderT[F, SkijaDrawState[G.Window], Unit](state =>
      I:
        state.canvas.drawRect(Rect.makeXYWH(x, y, width, height), new Paint().setColor(color))
        ()
    )
  end rectangle
end SkijaSimpleDrawApi

object SkijaSimpleDrawApi:
  def createForTests[F[+_] : {Impure as I, Async}] : Resource[F, (SimpleDrawApi[Float, ReaderT[F, SkijaDrawState[OglWindow], Unit]], Glfw[F] {type Window = OglWindow}, OglWindow, SkiaRenderTarget, Shaper)] =
    for
      dispatcher <- Dispatcher.sequential[F]
      glfw = GlfwImpl[F]([T] => (value : F[T]) => dispatcher.unsafeRunSync(value))
      dpi <- effect.Resource.eval(glfw.monitorScale(0))
      rt <- Skia.initSkia(500, 500, dpi._1)
      window <- glfw.createWindow("Test", Size(500, 500), true, true, true)
      shaper <- Resource.fromAutoCloseable(I(Shaper.make()))
    yield (new SkijaSimpleDrawApi(glfw), glfw, window, rt, shaper)
  end createForTests
end SkijaSimpleDrawApi
    