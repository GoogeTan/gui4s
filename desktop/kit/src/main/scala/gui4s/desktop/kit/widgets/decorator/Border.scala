package gui4s.desktop.kit.widgets.decorator

import cats.effect.*
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.skija.{Brush, SkPaint, StrokeOptions}
import io.github.humbleui.skija.PaintMode

extension[Event](widget : DesktopWidget[Event])
  def border(
    shape : Rect[Float] => Clip,
    brush: Brush,
    style : SkPaint = SkPaint(style = PaintMode.STROKE)
  ) : DesktopWidget[Event] =
    widget.withDrawOnlyBackground(
      PlacementEffect.getBounds.map:
        bounds =>
          val finiteBounds = bounds.map(_.getUnsafe)
          Sized(
            Draw.drawBorder(shape(finiteBounds), finiteBounds, brush, style),
            finiteBounds,
          )
    )
  end border

  def border(
    shape: Rect[Float] => Clip,
    brush: Brush,
    stroke : StrokeOptions,
  ): DesktopWidget[Event] =
    border(shape, brush, SkPaint(style = PaintMode.STROKE, stroke = stroke)) 
  end border
end extension