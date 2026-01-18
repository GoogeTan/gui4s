package gui4s.desktop.skija

import catnip.syntax.all.given
import cats.effect.Resource
import cats.effect.Sync
import io.github.humbleui.skija.BackendRenderTarget
import io.github.humbleui.skija.Canvas
import io.github.humbleui.skija.ColorSpace
import io.github.humbleui.skija.DirectContext
import io.github.humbleui.skija.FramebufferFormat
import io.github.humbleui.skija.PixelGeometry
import io.github.humbleui.skija.Surface
import io.github.humbleui.skija.SurfaceColorFormat
import io.github.humbleui.skija.SurfaceOrigin
import io.github.humbleui.skija.SurfaceProps
import io.github.humbleui.skija.shaper.Shaper

def createRenderTarget[
  IO[_] : Sync as S,
](
  context: DirectContext,
  width: Float,
  height: Float,
): Resource[IO, SkiaRenderTarget] =
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
  IO[_] : Sync as S,
](
  context: DirectContext,
  target: BackendRenderTarget,
  origin: SurfaceOrigin,
  colorFormat: SurfaceColorFormat,
  colorSpace: Option[ColorSpace],
  props: Option[SurfaceProps]
): Resource[IO, Surface] =
  Resource.eval(
    S.delay(
      Surface.wrapBackendRenderTarget(
        context,
        target,
        origin,
        colorFormat,
        colorSpace.orNull,
        props.orNull
      )
    )
  )
end createSurface

def getCanvas[IO[_] : Sync as S](surface: Surface): IO[Canvas] =
  S.delay(surface.getCanvas)
end getCanvas

def createGLRenderTarget[
  IO[_] : Sync as S,
](
  width: Int,
  height: Int,
  samples: Int = 0,
  stencil: Int = 8,
  fbId: Int = 0,
  fbFormat: Int
): Resource[IO, BackendRenderTarget] =
  Resource.fromAutoCloseable(
    S.delay(
      BackendRenderTarget.makeGL(
        width, height,
        samples,
        stencil,
        fbId,
        fbFormat
      )
    )
  )
end createGLRenderTarget

def createShaper[IO[_] : Sync as S]: Resource[IO, Shaper] =
  Resource.fromAutoCloseable(S.delay(Shaper.make()))
end createShaper

