package me.katze.gui4s.example
package draw.skija

import api.{DrawMonad, LayoutPlacementMeta}
import draw.{Drawable, drawLoopExceptionHandler}
import impl.{*, given}

import cats.data.ReaderT
import cats.effect.std.{Console, Dispatcher}
import cats.effect.{Async, Resource}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad, MonadError, Monoid, effect}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkiaRenderTarget, initSkia}
import me.katze.gui4s.widget.library.{LayoutDraw, TextDraw}
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL.createCapabilities

final case class SkijaDrawState[F[_], Window](context : DirectContext, glfw: Glfw[F, Window], window : Window, canvas : Canvas)

type SkijaDraw[F[_], Window] = ReaderT[F, SkijaDrawState[F, Window], Unit]

given [F[_] : {Impure as I, Monad}, Window]: DrawMonad[SkijaDraw[F, Window], Float] with
  def transition(canvas : Canvas, x : Float, y : Float) : F[Unit] =
    I:
      canvas.translate(x, y)
  end transition

  def moveAndBack[T](canvas: Canvas, x : Float, y : Float, value : F[T]) : F[T] =
    transition(canvas, x, y) *> value <* transition(canvas, -x, -y)
  end moveAndBack

  override def move(dx: Float, dy: Float, effect: SkijaDraw[F, Window]): SkijaDraw[F, Window] =
    ReaderT[F, SkijaDrawState[F, Window], Unit].apply(
      state =>
        moveAndBack(state.canvas, dx, dy, effect.run(state))
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
  (_, meta) =>
    ReaderT[F, SkijaDrawState[F, Window], Unit](
      state =>
        I:
          state.canvas.drawTextBlob(meta.textBlob, 0, 0, meta.paint)
    )
end skijaTextDraw

def flush[F[_] : {Monad, Impure as I}, Window]: ReaderT[F, SkijaDrawState[F, Window], Unit] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    I(state.context.flush())
      *> state.glfw.swapBuffers(state.window)
      *> state.glfw.pollEvents
      *> I(state.canvas.clear(0xFFFFFFFF))
  )
end flush

final case class SkijaBackend[F[_], Window](
                                              glfw : Glfw[F, Window],
                                              window: Window,
                                              renderTarget : SkiaRenderTarget,
                                              globalDispatcher : Dispatcher[F],
                                              globalShaper : Shaper,
                                            ):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    glfw.windowSize(window).map(a => new Bounds(a.width, a.height))
  end windowBounds
end SkijaBackend

object SkijaSimpleDrawApi:
  def createForTests[F[+_] : {Impure as I, Async}] : Resource[F, SkijaBackend[F, OglWindow]] =
    val windowSize = me.katze.gui4s.glfw.Size(640, 480)

    for
      dispatcher <- Dispatcher.sequential[F]
      glfw <- GlfwImpl[F](dispatcher)
      _ <- Resource.eval(glfw.createPrintErrorCallback)
      window <- glfw.createWindow(
        "Skija Text Example",
        windowSize,
        visible = true,
        resizeable = false,
        debugContext = false
      )
      _ <- Resource.eval(glfw.createOGLContext(window, I(createCapabilities())))
      rt <- initSkia(windowSize.width, windowSize.height, 1)
      shaper <- Resource.fromAutoCloseable(I(Shaper.make()))
    yield SkijaBackend(glfw, window, rt, dispatcher, shaper)
  end createForTests
end SkijaSimpleDrawApi


def skijaDrawLoop[F[+_] : {Console, Impure}, Window](backend : SkijaBackend[F, Window])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler)(
      currentWidget.flatMap(widget =>
        (widget.draw |+| flush[F, Window]).apply(SkijaDrawState(backend.renderTarget.directContext, backend.glfw, backend.window, backend.renderTarget.canvas))
      )
    )
end skijaDrawLoop
