package me.katze.gui4s.example
package draw.swing

import api.impl.LayoutPlacementMeta

import cats.Id
import me.katze.gui4s.layout.{Measurable, Sized}
import me.katze.gui4s.widget.library.TextPlacement

given swingTextPlacement[MeasurementUnit : Fractional]: TextPlacement[Any, Any, Measurable[Id, MeasurementUnit, LayoutPlacementMeta[MeasurementUnit]]] with
  override def sizeText(text: String, shaper : Any, options: Any): Measurable[Id, MeasurementUnit, LayoutPlacementMeta[MeasurementUnit]] =
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
