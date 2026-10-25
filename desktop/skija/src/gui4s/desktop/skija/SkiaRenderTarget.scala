package gui4s.desktop.skija

import io.github.humbleui.skija.BackendRenderTarget
import io.github.humbleui.skija.Canvas
import io.github.humbleui.skija.DirectContext
import io.github.humbleui.skija.Surface

final case class SkiaRenderTarget(
                                    directContext: DirectContext,
                                    target: BackendRenderTarget,
                                    surface: Surface,
                                    canvas: Canvas,
                                  )