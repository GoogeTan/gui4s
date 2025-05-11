package me.katze.gui4s.example
package draw.skija

import api.{DrawMonad, LayoutPlacementMeta}
import draw.{Drawable, drawLoopExceptionHandler}
import impl.{*, given}

import cats.data.ReaderT
import cats.effect.std.{Console, Dispatcher}
import cats.effect.{Async, ExitCode, Resource}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad, MonadError, Monoid, effect}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkiaRenderTarget, SkijaDraw, SkijaDrawState, drawFrame, flush, initSkia, moveAndBack}
import me.katze.gui4s.widget.library.{LayoutDraw, TextDraw}
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL.createCapabilities

given [F[_] : {Impure as I, Monad}, Window]: DrawMonad[SkijaDraw[F, Window], Float] with
    override def drawAt(x: Float, y: Float, effect: SkijaDraw[F, Window]): SkijaDraw[F, Window] =
      ReaderT[F, SkijaDrawState[F, Window], Unit](state => moveAndBack(state.canvas, x, y, effect.run(state)))
    end drawAt
end given

given skijaLayoutDraw[F[_] : {Impure, Monad}, Window]: LayoutDraw[SkijaDraw[F, Window], LayoutPlacementMeta[Float]] =
  layoutDrawImpl[SkijaDraw[F, Window], Float]
end skijaLayoutDraw

given skijaTextDraw[F[_] : Impure as I, Window]: TextDraw[SkijaDraw[F, Window], SkijaPlacedText] =
  (_, meta) =>
    ReaderT[F, SkijaDrawState[F, Window], Unit](
      state =>
        I:
          state.canvas.drawTextBlob(meta.textBlob, 0, 0, meta.paint)
    )
end skijaTextDraw

final case class SkijaBackend[F[_], Window](
                                              glfw : Glfw[F, Window],
                                              window: Window,
                                              renderTarget : SkiaRenderTarget,
                                              globalShaper : Shaper,
                                            ):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    glfw.frameBufferSize(window).map(a => new Bounds(a.width, a.height))
  end windowBounds

  def drawState : SkijaDrawState[F, Window] =
    SkijaDrawState(renderTarget.directContext, glfw, window, renderTarget.canvas)
  end drawState
end SkijaBackend

object SkijaSimpleDrawApi:
  def currentMonitorScale[F[_] : Monad, Window](glfw : Glfw[F, Window]) : F[Float] =
    glfw.currentMonitor >>= glfw.monitorScale
  end currentMonitorScale

  def createForTests[F[+_] : {Impure as I, Async}] : Resource[F, SkijaBackend[F, OglWindow]] =
    val windowSize = me.katze.gui4s.glfw.Size(640, 480)

    for
      dispatcher <- Dispatcher.sequential[F]
      glfw <- GlfwImpl[F](dispatcher)
      _ <- glfw.createPrintErrorCallback
      window <- glfw.createWindow(
        "Skija Text Example",
        windowSize,
        visible = true,
        resizeable = true, // TODO если разрешиь, то при изменении размеров окна, содержимое просто сжимается  всё
        debugContext = false
      )
      _ <- Resource.eval(glfw.createOGLContext(window, I(createCapabilities())))
      scale <- Resource.eval(currentMonitorScale(glfw))
      rt <- initSkia(windowSize.width, windowSize.height, scale)
      shaper <- Resource.fromAutoCloseable(I(Shaper.make()))
    yield SkijaBackend(glfw, window, rt, shaper)
  end createForTests
end SkijaSimpleDrawApi


def skijaDrawLoop[F[+_] : {Console, Impure}, Window](backend : SkijaBackend[F, Window])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.glfw.shouldNotClose(backend.window))(
      currentWidget.flatMap(widget =>
        drawFrame(widget.draw)(backend.drawState)
      )
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
