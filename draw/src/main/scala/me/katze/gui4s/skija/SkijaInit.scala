package me.katze.gui4s.skija

import cats.effect.Resource
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.skija.SkiaRenderTarget

trait SkijaInit[F[_]]:
  def createDirectContext: Resource[F, DirectContext]
  
  def createRenderTarget(
    context: DirectContext, 
    width: Float, 
    height: Float, 
  ): F[SkiaRenderTarget]
  
  def createSurface(
    context: DirectContext,
    target: BackendRenderTarget,
    origin: SurfaceOrigin,
    colorFormat: SurfaceColorFormat,
    colorSpace: Option[ColorSpace] = None,
    props: Option[SurfaceProps] = None
  ): Resource[F, Surface]
  
  def getCanvas(surface: Surface): F[Canvas]
  
  def createGLRenderTarget(
    width: Int,
    height: Int,
    samples: Int = 0,
    stencil: Int = 8,
    fbId: Int = 0,
    fbFormat: Int
  ): Resource[F, BackendRenderTarget]
  
  def createShaper : Resource[F, Shaper]
end SkijaInit