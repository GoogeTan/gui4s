package me.katze.gui4s.example
package draw.skija

import api.impl.LayoutPlacementMeta

import io.github.humbleui.skija.TextBlob
import me.katze.gui4s.widget.library.{LayoutDraw, TextDraw}
import api.impl.DrawMonad
import draw.{Drawable, SimpleDrawApi, TextStyle, drawLoopExceptionHandler}
import impl.{*, given}

import cats.data.ReaderT
import cats.effect.{Async, IO, Resource}
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
import me.katze.gui4s.skija.{TestFuncs, *} 
import org.lwjgl.opengl.GL

final case class SkijaDrawState[F[_], Window](context : DirectContext, glfw: Glfw[F, Window], window : Window, canvas : Canvas)

type SkijaDraw[F[_], Window] = ReaderT[F, SkijaDrawState[F, Window], Unit]

given [F[_] : {Impure, Monad}, Window]: DrawMonad[SkijaDraw[F, Window], Float] with
  override def move(dx: Float, dy: Float, effect: SkijaDraw[F, Window]): SkijaDraw[F, Window] =
    ReaderT[F, SkijaDrawState[F, Window], Unit].apply(
      state =>
        TestFuncs[F].moveAndBack(state.canvas, dx, dy, effect.run(state))
    )
end given

given[F[_] : Applicative, Value]: Monoid[SkijaDraw[F, Value]] with
  override def empty: SkijaDraw[F, Value] = ReaderT.pure[F, SkijaDrawState[F, Value], Unit](())

  override def combine(x: SkijaDraw[F, Value], y: SkijaDraw[F, Value]): SkijaDraw[F, Value] =
    ReaderT[F, SkijaDrawState[F, Value], Unit]:
      state => 
        x(state) *> y(state)
  end combine
end given

given skijaLayoutDraw[F[_] : {Impure, Monad}, Window]: LayoutDraw[SkijaDraw[F, Window], LayoutPlacementMeta[Float]] =
  layoutDrawImpl[SkijaDraw[F, Window], Float]

given skijaTextDraw[F[_] : Impure as I, Window]: TextDraw[SkijaDraw[F, Window], SkijaPlacedText] =
  (text, meta) =>
    ReaderT[F, SkijaDrawState[F, Window], Unit](
      state =>
        I:
          state.canvas.drawTextBlob(meta.textBlob, 0, 0, meta.paint)
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
                                      renderTarget : SkiaRenderTarget,
                                      globalDispatcher : Dispatcher[F],
                                      globalShaper : Shaper,
                                    ):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    glfw.windowSize(window).map(a => new Bounds(a.width, a.height))
  end windowBounds
end SkijaBackend

object SkijaSimpleDrawApi:
  def createForTests[F[+_] : {Impure as I, Async}] : Resource[F, SkijaBackend[F]] =
    for
      _ <- initGLFW
      config = WindowConfig(640, 480, "Skija Text Example")
      window <- createWindow(config)
      _ <- Resource.eval(setupOpenGL(window))
      resources <- createRenderResources(config.width, config.height)
      dispatcher <- Dispatcher.sequential[F]
      rt <- Skia.initSkia(config.width, config.height, 1)
      shaper <- Resource.fromAutoCloseable(I(Shaper.make()))
    yield SkijaBackend(glfw, window, rt, dispatcher, shaper)
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
