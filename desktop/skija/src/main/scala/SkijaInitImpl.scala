package gui4s.desktop.skija

import cats.effect.Sync
import cats.syntax.all.*
import cats.{Monad, ~>}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{BackendRenderTarget, Canvas, ColorSpace, DirectContext, FramebufferFormat, PixelGeometry, Surface, SurfaceColorFormat, SurfaceOrigin, SurfaceProps}

/** Реализация интерфейса для работы с Skija.
 * @tparam IO Эффект, в котором выполняются операции
 */
final class SkijaInitImpl[IO[_]: Sync as S, Resource[_] : Monad](eval : IO ~> Resource, fromAutoCloseable : [T <: AutoCloseable] => IO[T] => Resource[T]) extends SkijaInit[IO, Resource]:
  override def createDirectContext: Resource[DirectContext] =
    fromAutoCloseable(S.delay(DirectContext.makeGL()))
  end createDirectContext

  override def createRenderTarget(
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
      canvas <- eval(getCanvas(surface))
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
                            ): Resource[Surface] =
    fromAutoCloseable(
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
                                    ): Resource[BackendRenderTarget] =
    fromAutoCloseable(
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

  override def createShaper: Resource[Shaper] =
    fromAutoCloseable(S.delay(Shaper.make()))
  end createShaper
end SkijaInitImpl

