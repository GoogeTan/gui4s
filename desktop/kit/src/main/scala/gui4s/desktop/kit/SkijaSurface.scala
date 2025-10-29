package gui4s.desktop.kit

import catnip.ResourceCell
import cats.data.Kleisli
import cats.effect.*
import cats.~>
import glfw4s.core.pure.*
import gui4s.core.geometry.Rect
import gui4s.desktop.skija.*

final case class SkijaSurface[
  IO[_]
](
  renderTargetCell : ResourceCell[IO, SkiaRenderTarget]
):
  def getRenderTarget : Resource[IO, SkiaRenderTarget] =
    renderTargetCell.get
  end getRenderTarget

  def drawFrame[G[_]](
                       frame : SkiaRenderTarget => G[Unit],
                       lift : IO ~> G
                     )(using MonadCancel[IO, Throwable], MonadCancel[G, Throwable]) : G[Unit] =
    getRenderTarget.mapK(lift).use(frame)
  end drawFrame

  def drawFrameKleisli[G[_]](
                              frame : Kleisli[G, SkiaRenderTarget, Unit],
                              lift : IO ~> G
                            )(using MonadCancel[IO, Throwable], MonadCancel[G, Throwable]) : G[Unit] =
    drawFrame(frame.run, lift)
  end drawFrameKleisli

  def recreateRenderTarget(newSize : Rect[Float])(using Sync[IO]) : IO[Unit] =
    renderTargetCell.evalReplace(state =>
        createRenderTarget[IO](state.directContext, newSize.width, newSize.height)
    )
  end recreateRenderTarget
end SkijaSurface

object SkijaSurface:
  def create[
    IO[_] : Async
  ](
     width : Int,
     height : Int
    ): Resource[IO, SkijaSurface[IO]] =
    for
      context <- createDirectContext[IO]
      renderTargetCell <- ResourceCell.blocking[IO, SkiaRenderTarget](
        gui4s.desktop.skija.createRenderTarget(
          context = context,
          width = width,
          height = height
        ),
      )
    yield SkijaSurface(renderTargetCell)
  end create

  def create[
    IO[_] : Async,
    CallbackIO[_] : Async,
    Monitor,
    Window,
  ](
     window  : Window,
     glfw : PostInit[IO, CallbackIO[Unit], Monitor, Window],
     liftToIO : CallbackIO ~> IO
    ) : Resource[IO, SkijaSurface[CallbackIO]] =
    Resource.eval(glfw.getFramebufferSize(window))
      .flatMap((width, height) => create[CallbackIO](width, height).mapK(liftToIO))
  end create
end SkijaSurface