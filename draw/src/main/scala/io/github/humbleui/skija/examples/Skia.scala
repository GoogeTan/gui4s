package io.github.humbleui.skija.examples

import cats.effect.*
import cats.syntax.all.*
import io.github.humbleui.skija.*
import me.katze.gui4s.impure.Impure

object Skia:
  final case class SkiaRenderTarget(
                                      target : BackendRenderTarget,
                                      surface: Surface,
                                      canvas : Canvas
                                    )
  
  def initSkia[F[_] : {Impure, Sync}](context : DirectContext, width : Float, height : Float, dpi : Float) : Resource[F, SkiaRenderTarget] =
    for
      renderTarget <- makeGl(
        (width * dpi).toInt, (height * dpi).toInt, FramebufferFormat.GR_GL_RGBA8
      )
      surface <- wrapTargetIntoSurface(
        context,
        renderTarget,
        SurfaceOrigin.BOTTOM_LEFT,
        SurfaceColorFormat.RGBA_8888,
        Some(ColorSpace.getDisplayP3), // TODO load monitor profile
        Some(new SurfaceProps(PixelGeometry.RGB_H))
      )
      canvas <- Resource.eval(getOrMakeCanvas(surface))
    yield SkiaRenderTarget(renderTarget, surface, canvas)
  end initSkia

  def getOrMakeCanvas[F[_] : Impure as I](surface: Surface) : F[Canvas] =
    I.impure(surface.getCanvas)
  end getOrMakeCanvas
  
  def makeGl[F[_] : {Impure as I, Sync}](width : Int, height : Int, fbFormat : Int) : Resource[F, BackendRenderTarget] =
    Resource.fromAutoCloseable(
      I.impure:
        BackendRenderTarget.makeGL(
          width, height,
          /*samples*/ 0,
          /*stencil*/ 8,
          /*fbId*/ 0,
          fbFormat
        )
    )
  end makeGl
  
  def wrapTargetIntoSurface[F[_] : {Impure as I, Sync}](
                                                          context: DirectContext, 
                                                          rt: BackendRenderTarget,
                                                          origin: SurfaceOrigin,
                                                          colorFormat: SurfaceColorFormat,
                                                          colorSpace: Option[ColorSpace],
                                                          surfaceProps: Option[SurfaceProps]
                                                        ) : Resource[F, Surface] =
    Resource.fromAutoCloseable(
      I.impure:
        Surface.wrapBackendRenderTarget(context, 
          rt, 
          origin,
          colorFormat,
          colorSpace.orNull,
          surfaceProps.orNull
        )
    )
  end wrapTargetIntoSurface
end Skia
