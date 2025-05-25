package me.katze.gui4s.layout

import me.katze
final case class Placed[+MeasurementUnit, +T](value : T, x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit, width : MeasurementUnit, height : MeasurementUnit):
  def this(sized : Sized[MeasurementUnit, T], x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit) = this(sized.value, x, y, z, sized.width, sized.height)
end Placed
