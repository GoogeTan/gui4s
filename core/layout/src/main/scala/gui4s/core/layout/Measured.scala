package gui4s.core.layout

import gui4s.core.geometry.Rect

final case class Measured[MeasurementUnit, BoundUnit, T](
  value : T,
  size : Rect[MeasurementUnit],
  bounds : Rect[BoundUnit]
):
  def this(sized: Sized[MeasurementUnit, T], bounds : Rect[BoundUnit]) =
    this(sized.value, sized.size, bounds)
  end this

  def asSized : Sized[MeasurementUnit, T] = Sized(value, size)

  def map[B](f : T => B) : Measured[MeasurementUnit, BoundUnit, B] =
    copy(value = f(value))
  end map
end Measured
