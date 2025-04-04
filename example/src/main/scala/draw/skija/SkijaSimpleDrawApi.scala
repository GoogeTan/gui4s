package me.katze.gui4s.example
package draw.skija

import draw.{SimpleDrawApi, TextStyle}

import cats.Monad
import cats.data.ReaderT
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, DirectContext, Font, Paint, Typeface}
import io.github.humbleui.types.Rect
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure

final case class DrawState[Window](context : DirectContext, window : Window, canvas : Canvas, shaper: Shaper)

final case class SkijaSimpleDrawApi[F[_] : {Impure as I, Monad as M}, W](G : Glfw[F] { type Window  = W })
              extends SimpleDrawApi[Float, ReaderT[F, DrawState[W], Unit]]:
  override def beginDraw: ReaderT[F, DrawState[W], Unit] =
    ReaderT[F, DrawState[G.Window], Unit](_ => M.pure(()))
  end beginDraw

  override def endDraw: ReaderT[F, DrawState[W], Unit] =
    ReaderT[F, DrawState[G.Window], Unit](state =>
      I(state.context.flush()) *> G.swapBuffers(state.window)
    )
  end endDraw

  override def text(x: Float, y: Float, text: String, style: TextStyle): ReaderT[F, DrawState[W], Unit] =
    ReaderT[F, DrawState[G.Window], Unit](state =>
      I:
        val font = new Font(Typeface.makeDefault())
        val blob = state.shaper.shape(text, font.setSize(style.size))
        state.canvas.drawTextBlob(blob, x, y, new Paint().setColor(style.color))
        ()
    )
  end text

  override def rectangle(x: Float, y: Float, width: Float, height: Float, color: Int): ReaderT[F, DrawState[W], Unit] =
    ReaderT[F, DrawState[G.Window], Unit](state =>
      I:
        state.canvas.drawRect(Rect.makeXYWH(x, y, width, height), new Paint().setColor(color))
        ()
    )
  end rectangle
end SkijaSimpleDrawApi
