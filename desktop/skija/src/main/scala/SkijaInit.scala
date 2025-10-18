package gui4s.desktop.skija

import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper

trait SkijaInit[IO[_], Resource[_]]:
  def createDirectContext: Resource[DirectContext]
  
  def createRenderTarget(
    context: DirectContext, 
    width: Float, 
    height: Float, 
  ): Resource[SkiaRenderTarget]
  
  def createSurface(
    context: DirectContext,
    target: BackendRenderTarget,
    origin: SurfaceOrigin,
    colorFormat: SurfaceColorFormat,
    colorSpace: Option[ColorSpace] = None,
    props: Option[SurfaceProps] = None
  ): Resource[Surface]
  
  def getCanvas(surface: Surface): IO[Canvas]
  
  def createGLRenderTarget(
    width: Int,
    height: Int,
    samples: Int = 0,
    stencil: Int = 8,
    fbId: Int = 0,
    fbFormat: Int
  ): Resource[BackendRenderTarget]
  
  def createShaper : Resource[Shaper]
end SkijaInit