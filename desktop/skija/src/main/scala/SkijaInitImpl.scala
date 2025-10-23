package gui4s.desktop.skija

import catnip.resource.{EvalC, SyncResource}
import cats.Monad
import cats.syntax.all.*
import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{BackendRenderTarget, Canvas, ColorSpace, DirectContext, FramebufferFormat, PixelGeometry, Surface, SurfaceColorFormat, SurfaceOrigin, SurfaceProps}

def createDirectContext[Resource[_] : SyncResource as S]: Resource[DirectContext] =
  S.fromAutoCloseable(() => DirectContext.makeGL())
end createDirectContext

def createRenderTarget[
  IO[_] : Sync as S,
  Resource[_] : {Monad, SyncResource, EvalC[IO]}
](
  context: DirectContext,
  width: Float,
  height: Float,
): Resource[SkiaRenderTarget] =
  for
    renderTarget <- createGLRenderTarget(
      width = width.toInt,
      height = height.toInt,
      fbFormat = FramebufferFormat.GR_GL_RGBA8
    )
    surface <- createSurface(
      context,
      renderTarget,
      SurfaceOrigin.BOTTOM_LEFT,
      SurfaceColorFormat.RGBA_8888,
      Some(ColorSpace.getDisplayP3), // TODO load monitor profile
      Some(new SurfaceProps(PixelGeometry.RGB_H))
    )
    canvas <- getCanvas(surface).eval
  yield SkiaRenderTarget(context, renderTarget, surface, canvas)
end createRenderTarget

@SuppressWarnings(Array("org.wartremover.warts.Null"))
def createSurface[
  Resource[_] : SyncResource as S
](
  context: DirectContext,
  target: BackendRenderTarget,
  origin: SurfaceOrigin,
  colorFormat: SurfaceColorFormat,
  colorSpace: Option[ColorSpace],
  props: Option[SurfaceProps]
): Resource[Surface] =
  S.fromAutoCloseable(() =>
    Surface.wrapBackendRenderTarget(
      context,
      target,
      origin,
      colorFormat,
      colorSpace.orNull,
      props.orNull
    )
  )
end createSurface

def getCanvas[IO[_] : Sync as S](surface: Surface): IO[Canvas] =
  S.delay(surface.getCanvas)
end getCanvas

def createGLRenderTarget[
  Resource[_] : SyncResource as S
](
  width: Int,
  height: Int,
  samples: Int = 0,
  stencil: Int = 8,
  fbId: Int = 0,
  fbFormat: Int
): Resource[BackendRenderTarget] =
  S.fromAutoCloseable(
    () =>
      BackendRenderTarget.makeGL(
        width, height,
        samples,
        stencil,
        fbId,
        fbFormat
      )
  )
end createGLRenderTarget

def createShaper[IO[_] : Sync as S]: Resource[IO, Shaper] =
  Resource.fromAutoCloseable(S.delay(Shaper.make()))
end createShaper

