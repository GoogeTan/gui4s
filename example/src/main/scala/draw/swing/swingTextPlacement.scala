package me.katze.gui4s.example
package draw.swing

import me.katze.gui4s.example.api.impl.LayoutPlacementMeta
import me.katze.gui4s.layout.{Measurable, Sized}
import me.katze.gui4s.widget.library.TextPlacement

given swingTextPlacement[MeasurementUnit : Fractional]: TextPlacement[Measurable[MeasurementUnit, LayoutPlacementMeta[MeasurementUnit]], Any] with
  override def sizeText(text: String, options: Any): Measurable[MeasurementUnit, LayoutPlacementMeta[MeasurementUnit]] =
    _ => Sized(
      LayoutPlacementMeta(
        Fractional[MeasurementUnit].zero, 
        Fractional[MeasurementUnit].zero
      ), 
      Fractional[MeasurementUnit].zero, 
      Fractional[MeasurementUnit].fromInt(17)
    )
  end sizeText
end swingTextPlacement
