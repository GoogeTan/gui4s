package me.katze.gui4s.example
package draw.skija

import api.LayoutPlacementMeta
import draw.{Drawable, drawLoopExceptionHandler}
import impl.given

import catnip.syntax.all.given
import cats.data.ReaderT
import cats.effect.std.{AtomicCell, Console, Dispatcher}
import cats.effect.{Async, ExitCode, Resource}
import cats.syntax.all.*
import cats.{Functor, Monad, MonadError}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.*
import org.lwjgl.opengl.GL.createCapabilities

def drawAt[F[_] : {Impure, Monad}, Window](original : SkijaDraw[F, Window], meta : LayoutPlacementMeta[Float]) : SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state => moveAndBack(state.canvas, meta.x, meta.y, original.run(state)))
end drawAt

def drawText[F[_] : Impure as I, Window](text : SkijaPlacedText) =
  ReaderT[F, SkijaDrawState[F, Window], Unit](
    state =>
      I:
        state.canvas.drawTextBlob(text.textBlob, 0, 0, text.paint)
  )
end drawText

def flush[F[_] : {Monad, Impure as I}, Window]: SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    I(state.context.flush())
      *> state.glfw.swapBuffers(state.window)
      *> I(state.canvas.clear(0xFFFFFFFF))
  )
end flush

final case class SkijaBackend[F[_], Window](
                                              private val glfw : Glfw[F, Window],
                                              private val window: Window,
                                              private val renderTargetCell : AtomicCell[F, SkiaRenderTarget],
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
        resizeable = false,
        debugContext = true
      )
      _ <- Resource.eval(glfw.createOGLContext(window, GlfwImpure(createCapabilities())))
      scale <- Resource.eval(glfw.primaryMonitor >>= glfw.monitorScale)
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
          _ <- c.println("It works 1")
          monitor <- glfw.windowMonitor(window)
          _ <- c.println("It works 2")
          dpi <- glfw.monitorScale(monitor)
          _ <- c.println("It works 3")
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
        backend.drawState((widget.draw |+| flush[F, Window]).run) *> backend.pollEvents
      )
    ).map(_.getOrElse(ExitCode.Success))
end skijaDrawLoop
