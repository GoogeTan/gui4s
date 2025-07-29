package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import cats.effect.{Async, Resource, Sync}
import io.github.humbleui.skija.{BackendRenderTarget, Canvas, ColorSpace, DirectContext, FramebufferFormat, PixelGeometry, Surface, SurfaceColorFormat, SurfaceOrigin, SurfaceProps}
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper

/** Реализация интерфейса для работы с Skija.
 * @tparam F Эффект, в котором выполняются операции
 * @param ffi Оборачивает грязные эффекты. Должен гарантировать исполнение кода на том же поткое, где и был создан контекст OGL(вероятно, первый(на mac os только первый))
 */
final class SkijaInitImpl[F[_]: Async](ffi : ForeighFunctionInterface[F]) extends SkijaInit[F]:
  override def createDirectContext: Resource[F, DirectContext] =
    Resource.fromAutoCloseable(
      ffi.delay(DirectContext.makeGL())
    )
  end createDirectContext

  override def createRenderTarget(
                                    context: DirectContext,
                                    width: Float,
                                    height: Float,
                                    dpi: Float
                                  ): F[SkiaRenderTarget] =
    val inner: Resource[F, SkiaRenderTarget] = for
      renderTarget <- createGLRenderTarget(
        width = (width * dpi).toInt,
        height = (height * dpi).toInt,
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
      canvas <- Resource.eval(getCanvas(surface))
    yield SkiaRenderTarget(context, renderTarget, surface, canvas, dpi)
    inner.allocated.map((a, _) => a)
  end createRenderTarget

  override def createSurface(
                              context: DirectContext,
                              target: BackendRenderTarget,
                              origin: SurfaceOrigin,
                              colorFormat: SurfaceColorFormat,
                              colorSpace: Option[ColorSpace],
                              props: Option[SurfaceProps]
                            ): Resource[F, Surface] =
    Resource.fromAutoCloseable(
      ffi.delay(
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

  override def getCanvas(surface: Surface): F[Canvas] =
    ffi(surface.getCanvas)
  end getCanvas

  override def createGLRenderTarget(
                                      width: Int,
                                      height: Int,
                                      samples: Int,
                                      stencil: Int,
                                      fbId: Int,
                                      fbFormat: Int
                                    ): Resource[F, BackendRenderTarget] =
    Resource.fromAutoCloseable(
      ffi.delay(
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

  def closeRenderTarget(target: SkiaRenderTarget): F[Unit] =
    ffi.delay:
      target.target.close()
      target.surface.close()
  end closeRenderTarget

  override def createShaper: Resource[F, Shaper] =
    Resource.fromAutoCloseable(ffi.delay(Shaper.make()))
  end createShaper
end SkijaInitImpl

object SkijaInitImpl:
  def apply[F[_]: Async](impure : ForeighFunctionInterface[F]): F[SkijaInitImpl[F]] =
    Sync[F].delay(new SkijaInitImpl[F](impure))
  end apply
end SkijaInitImpl