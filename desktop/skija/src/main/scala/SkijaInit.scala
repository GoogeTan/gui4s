package gui4s.desktop.skija

import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import cats.effect.Resource

trait SkijaInit[IO[_]]:
  def createDirectContext: Resource[IO, DirectContext]
  
  def createRenderTarget(
    context: DirectContext, 
    width: Float, 
    height: Float, 
  ): Resource[IO, SkiaRenderTarget]
  
  def createSurface(
    context: DirectContext,
    target: BackendRenderTarget,
    origin: SurfaceOrigin,
    colorFormat: SurfaceColorFormat,
    colorSpace: Option[ColorSpace] = None,
    props: Option[SurfaceProps] = None
  ): Resource[IO, Surface]
  
  def getCanvas(surface: Surface): IO[Canvas]
  
  def createGLRenderTarget(
    width: Int,
    height: Int,
    samples: Int = 0,
    stencil: Int = 8,
    fbId: Int = 0,
    fbFormat: Int
  ): Resource[IO, BackendRenderTarget]
  
  def createShaper : Resource[IO, Shaper]
end SkijaInit