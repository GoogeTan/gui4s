package me.katze.gui4s.skija

import cats.effect.*
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.github.humbleui.skija.{BackendRenderTarget, Canvas, ColorSpace, DirectContext, FramebufferFormat, PixelGeometry, Surface, SurfaceColorFormat, SurfaceOrigin, SurfaceProps}
import me.katze.gui4s.impure.Impure

final case class SkiaRenderTarget(
                                    directContext: DirectContext,
                                    target: BackendRenderTarget,
                                    surface: Surface,
                                    canvas: Canvas
                                  ):
  def dealloc[F[_] : Impure as I] : F[Unit] = I:
    directContext.close()
    target.close()
    surface.close()
  end dealloc
end SkiaRenderTarget

def createSkiaRenderTarget[F[_] : {Impure, Async}](width : Float, height : Float, dpi : Float) : F[SkiaRenderTarget] =
  val inner: Resource[F, SkiaRenderTarget] = for
    context <- makeContext
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
  yield SkiaRenderTarget(context, renderTarget, surface, canvas)
  inner.allocated.map((a, _) => a)
end createSkiaRenderTarget

// TODO Refactor this hell
def initSkia[F[_] : {Impure, Async}](width: Float, height: Float, dpi: Float): Resource[F, AtomicCell[F, SkiaRenderTarget]] =
  val effect = createSkiaRenderTarget(width, height, dpi)
                .map(renderTarget => Resource.make(AtomicCell[F].of(renderTarget))(_.get.flatMap(_.dealloc)))

  Resource.eval(effect).flatten
end initSkia

def makeContext[F[_] : {Impure as I, Sync}]: Resource[F, DirectContext] =
  Resource.fromAutoCloseable(
    I.impure:
      DirectContext.makeGL()
  )

def getOrMakeCanvas[F[_] : Impure as I](surface: Surface): F[Canvas] =
  I.impure(surface.getCanvas)
end getOrMakeCanvas

def makeGl[F[_] : {Impure as I, Sync}](width: Int, height: Int, fbFormat: Int): Resource[F, BackendRenderTarget] =
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
                                                      ): Resource[F, Surface] =
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