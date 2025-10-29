package gui4s.desktop.skija

import io.github.humbleui.skija.{BackendRenderTarget, Canvas, DirectContext, Surface}

final case class SkiaRenderTarget(
                                    directContext: DirectContext,
                                    target: BackendRenderTarget,
                                    surface: Surface,
                                    canvas: Canvas,
                                  )