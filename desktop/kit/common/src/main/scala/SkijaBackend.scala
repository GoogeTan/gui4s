package gui4s.desktop.kit
package common

import cats.Monad
import catnip.ResourceCell
import catnip.resource.*
import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.effect.*
import cats.effect.std.QueueSink
import cats.syntax.all.*
import cats.{Apply, Functor, ~>}
import glfw4s.core.pure.*
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.desktop.skija.*
import gui4s.desktop.skija.DirectContext.flush
import io.github.humbleui.skija.Canvas

final case class SkijaBackend[
  IO[_],
  Resource[_],
  CallbackIO[_],
  Monitor,
  Window,
  DownEvent
](
   queue : QueueSink[CallbackIO, DownEvent],
   glfw : PostInit[IO, Resource, CallbackIO[Unit], Monitor, Window],
   window: Window,
   renderTargetCell : ResourceCell[CallbackIO, Resource, SkiaRenderTarget],
   liftToIO : CallbackIO ~> IO
):
  def windowBounds(using Functor[IO]) : IO[Rect[Float]] =
    glfw.getWindowSize(window).map(Rect(_, _))
  end windowBounds

  def mousePosition(using Functor[IO]) : IO[Point2d[Float]] =
    glfw.glfwGetCursorPos(window).map(Point2d(_, _))
  end mousePosition

  def windowShouldNotClose(using Functor[IO]) : IO[Boolean] =
    glfw.shouldNotWindowClose(window)
  end windowShouldNotClose

  def drawFrame[T](using Sync[IO])(f : Canvas => IO[T]) : IO[T] =
    renderTargetCell.eval(
      renderTarget =>
        f(renderTarget.canvas)
          <* flush(renderTarget.directContext)
          <* glfw.swapBuffers(window)
          <* glfw.pollEvents(),
      liftToIO
    )
  end drawFrame

  def drawFrame[T](using Sync[IO])(f : ReaderT[IO, Canvas, T]) : IO[T] =
    drawFrame(f.run)
  end drawFrame

  def raiseEventInCallback(event: DownEvent): CallbackIO[Unit] =
    queue.offer(event)
  end raiseEventInCallback

  def raiseEvent(event : DownEvent) : IO[Unit] =
    liftToIO(raiseEventInCallback(event))
  end raiseEvent

  def recreateRenderTarget(newSize : Rect[Float])(using Sync[CallbackIO], Monad[Resource], SyncResource[Resource], Eval[Resource, CallbackIO]): CallbackIO[Unit] =
    renderTargetCell.evalReplace(state =>
      createRenderTarget[CallbackIO, Resource](state.directContext, newSize.width, newSize.height)
    )
  end recreateRenderTarget
end SkijaBackend

object SkijaBackend:
  def create[
    IO[_] : Async,
    Resource[_] : {Monad, SyncResource, EvalC[CallbackIO], EvalC[IO]},
    CallbackIO[_] : Async,
    Monitor,
    Window,
    DownEvent
  ](
     queue : QueueSink[CallbackIO, DownEvent],
     glfw : PostInit[IO, Resource, CallbackIO[Unit], Monitor, Window],
     createWindow : Resource[Window],
     createRenderTarget : Window => Resource[ResourceCell[CallbackIO, Resource, SkiaRenderTarget]],
     liftIO : CallbackIO ~> IO
    ): Resource[SkijaBackend[IO, Resource, CallbackIO, Monitor, Window, DownEvent]] =
    for
      window <- createWindow
      renderTargetCell <- createRenderTarget(window)
      backend = SkijaBackend(queue, glfw, window, renderTargetCell, liftIO)
      _ <- registerCallbacksForMaintainingRenderTarget(glfw, window, backend.recreateRenderTarget).eval
    yield backend
  end create

  def createRenderTarget[
    IO[_] : Async,
    Resource[_] : {Monad, MakeC[IO], AllocateC[IO], SyncResource, EvalC[IO]},
  ](
    width : Int,
    height : Int
   ) : Resource[ResourceCell[IO, Resource, SkiaRenderTarget]] =
    for
      context <- createDirectContext[Resource]
      renderTargetCell <- ResourceCell.blocking[IO, Resource, SkiaRenderTarget](
          gui4s.desktop.skija.createRenderTarget(
            context = context,
            width = width,
            height = height
          ),
        )
    yield renderTargetCell
  end createRenderTarget


  def registerCallbacksForMaintainingRenderTarget[
    IO[_] : Apply,
    Resource[_],
    CallbackResult,
    Monitor,
    Window,
  ](
     glfw : PostInit[IO, Resource, CallbackResult, Monitor, Window],
     window: Window,
     recreateRenderTarget : (size : Rect[Float]) => CallbackResult
    ): IO[Unit] =
    glfw.setFramebufferSizeCallback(window,
      (window, width, height) =>
        recreateRenderTarget(Rect(width, height))
    )
  end registerCallbacksForMaintainingRenderTarget
end SkijaBackend