package me.katze.gui4s.example
package draw.skija

import api.{DrawMonad, LayoutPlacementMeta}
import draw.{Drawable, drawLoopExceptionHandler}
import impl.{*, given}

import cats.data.ReaderT
import cats.effect.std.{AtomicCell, Console, Dispatcher}
import cats.effect.{Async, ExitCode, Ref, Resource}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad, MonadError, Monoid, effect}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkiaRenderTarget, SkijaDraw, SkijaDrawState, createSkiaRenderTarget, initSkia, moveAndBack}
import me.katze.gui4s.widget.library.{LayoutDraw, TextDraw}
import org.lwjgl.glfw.GLFW.glfwGetPrimaryMonitor
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

def flush[F[_] : {Monad, Impure as I}, Window]: SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    I(state.context.flush())
      *> state.glfw.swapBuffers(state.window)
      *> I(state.canvas.clear(0xFFFFFFFF))
  )
end flush

final case class SkijaBackend[F[_], Window](
                                              glfw : Glfw[F, Window],
                                              window: Window,
                                              renderTarget : AtomicCell[F, SkiaRenderTarget],
                                              globalDispatcher : Dispatcher[F],
                                              globalShaper : Shaper,
                                            ):
  def windowBounds(using Functor[F]) : F[Bounds[Float]] =
    glfw.frameBufferSize(window).map(a => new Bounds(a.width, a.height))
  end windowBounds

  def windowShouldNotClose(using M : Monad[F]) : F[Boolean] =
    glfw.shouldNotClose(window)
  end windowShouldNotClose

  def drawState[T](using M : Monad[F])(f : SkijaDrawState[F, Window] => F[T]) : F[T] =
    renderTarget.evalModify(
      rt =>
        f(SkijaDrawState(rt.directContext, glfw, window, rt.canvas)).map(a => (rt, a))
    )
  end drawState
end SkijaBackend

object SkijaSimpleDrawApi:
  def createForTests[F[+_] : {Async, Console}](GlfwImpure : Impure[F], CommonImpure : Impure[F]) : Resource[F, SkijaBackend[F, OglWindow]] =
    val windowSize = me.katze.gui4s.glfw.Size(640, 480)
    for
      dispatcher <- Dispatcher.sequential[F]
      glfw : Glfw[F, OglWindow] <- GlfwImpl[F](dispatcher)(using GlfwImpure)
      _ <- glfw.createPrintErrorCallback
      window <- glfw.createWindow(
        "Skija Text Example",
        windowSize,
        visible = true,
        resizeable = true,
        debugContext = false
      )
      _ <- Resource.eval(glfw.createOGLContext(window, GlfwImpure(createCapabilities())))
      scale <- Resource.eval(glfw.currentMonitor >>= glfw.monitorScale)
      rt : AtomicCell[F, SkiaRenderTarget] <- initSkia(windowSize.width, windowSize.height, scale)(using GlfwImpure)
      _ <- Resource.eval(windowResizedCallback(glfw, window, rt)(using CommonImpure))
      shaper <- Resource.fromAutoCloseable(CommonImpure(Shaper.make()))
    yield SkijaBackend(glfw, window, rt, dispatcher, shaper)
  end createForTests

  def windowResizedCallback[F[_] : {Impure as I, Async, Console as c}, Window](glfw : Glfw[F, Window], window : Window, targetCell : AtomicCell[F, SkiaRenderTarget]): F[Unit] =
    glfw.windowResizeCallback(window, newSize =>
      targetCell.evalUpdate(state =>
        for
          //_ <- state.dealloc
          dpi <- glfw.windowMonitor(window) >>= glfw.monitorScale
          newRenderTarget <- createSkiaRenderTarget(state.directContext, newSize.width, newSize.height, dpi)
        yield newRenderTarget
      )
    )
  end windowResizedCallback
end SkijaSimpleDrawApi

def skijaDrawLoop[F[+_] : {Console, Impure}, Window](backend : SkijaBackend[F, Window])(using MonadError[F, Throwable]) : DrawLoop[F, Drawable[SkijaDraw[F, Window]]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler, backend.windowShouldNotClose)(
      currentWidget.flatMap(widget =>
        backend.drawState((widget.draw |+| flush[F, Window]).run) *> backend.glfw.pollEvents
      )
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
