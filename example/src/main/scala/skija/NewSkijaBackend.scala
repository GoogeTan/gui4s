package me.katze.gui4s.example
package skija

import io.github.humbleui.skija.{BackendRenderTarget, Canvas, DirectContext, Surface}

final class NewSkijaBackend(
                              directContext: DirectContext,
                              target: BackendRenderTarget,
                              surface: Surface,
                              canvas: Canvas,
                            )
