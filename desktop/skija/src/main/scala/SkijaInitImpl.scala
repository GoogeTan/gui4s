package gui4s.desktop.skija

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{BackendRenderTarget, Canvas, ColorSpace, DirectContext, FramebufferFormat, PixelGeometry, Surface, SurfaceColorFormat, SurfaceOrigin, SurfaceProps}

/** Реализация интерфейса для работы с Skija.
 * @tparam IO Эффект, в котором выполняются операции
 */
final class SkijaInitImpl[IO[_]: Sync as S] extends SkijaInit[IO]:
  override def createDirectContext: Resource[IO, DirectContext] =
    Resource.fromAutoCloseable(S.delay(DirectContext.makeGL()))
  end createDirectContext

  override def createRenderTarget(
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
      canvas <- Resource.eval(getCanvas(surface))
    yield SkiaRenderTarget(context, renderTarget, surface, canvas)
  end createRenderTarget

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override def createSurface(
                              context: DirectContext,
                              target: BackendRenderTarget,
                              origin: SurfaceOrigin,
                              colorFormat: SurfaceColorFormat,
                              colorSpace: Option[ColorSpace],
                              props: Option[SurfaceProps]
                            ): Resource[IO, Surface] =
    Resource.fromAutoCloseable(
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

  override def getCanvas(surface: Surface): IO[Canvas] =
    S.delay(surface.getCanvas)
  end getCanvas

  override def createGLRenderTarget(
                                      width: Int,
                                      height: Int,
                                      samples: Int,
                                      stencil: Int,
                                      fbId: Int,
                                      fbFormat: Int
                                    ): Resource[IO, BackendRenderTarget] =
    Resource.fromAutoCloseable(
      S.delay:
        BackendRenderTarget.makeGL(
          width, height,
          samples,
          stencil,
          fbId,
          fbFormat
        )
    )
  end createGLRenderTarget

  override def createShaper: Resource[IO, Shaper] =
    Resource.fromAutoCloseable(S.delay(Shaper.make()))
  end createShaper
end SkijaInitImpl

