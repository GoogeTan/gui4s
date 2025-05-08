package me.katze.gui4s.layout

import me.katze
final case class Placed[+MeasurementUnit, +T](value : T, x : MeasurementUnit, y : MeasurementUnit, width : MeasurementUnit, height : MeasurementUnit):
  def this(sized : Sized[MeasurementUnit, T], x : MeasurementUnit, y : MeasurementUnit) = this(sized.value, x, y, sized.width, sized.height)
end Placed
