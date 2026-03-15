package gui4s.desktop.skija

import io.github.humbleui.skija.PaintStrokeCap
import io.github.humbleui.skija.PaintStrokeJoin

case class StrokeOptions(
  width: Float = 0.0f, // 0.0f = "Hairline" (always 1px wide)
  miterLimit: Float = 4.0f,
  cap: PaintStrokeCap = PaintStrokeCap.BUTT,
  join: PaintStrokeJoin = PaintStrokeJoin.MITER
):
  def applyToSkia(paint: Paint): Paint = 
    paint.setStrokeWidth(width)
      .setStrokeMiter(miterLimit)
      .setStrokeCap(cap)
      .setStrokeJoin(join)