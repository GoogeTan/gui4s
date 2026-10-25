package gui4s.desktop.kit

import catnip.ResourceCell
import cats.effect.*
import cats.~>

import gui4s.core.geometry.Rect

import gui4s.desktop.skija.*
import gui4s.desktop.skija.DirectContext.createDirectContext

final case class SkijaSurface[
  IO[_]
](
  renderTargetCell : ResourceCell[IO, SkiaRenderTarget]
):
  def getRenderTarget : Resource[IO, SkiaRenderTarget] =
    renderTargetCell.get
  end getRenderTarget

  /*
  def drawFrame[G[_]](
                       frame : SkiaRenderTarget => G[Unit],
                       lift : IO ~> G
                     )(using MonadCancel[IO, Throwable], MonadCancel[G, Throwable]) : G[Unit] =
    getRenderTarget.mapK(lift).use(frame)
  end drawFrame*/

  def drawFrame[G[_]](
    frame: SkiaRenderTarget => G[Unit],
    lift: G ~> IO
  )(using MonadCancel[IO, Throwable]): IO[Unit] =
    getRenderTarget.use(target => lift(frame(target)))
  end drawFrame

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
end SkijaSurface