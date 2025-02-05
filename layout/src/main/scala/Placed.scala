package me.katze.gui4s.layout

import me.katze
final case class Placed[+MeasurementUnit, +T](value : T, x : MeasurementUnit, y : MeasurementUnit, width : MeasurementUnit, height : MeasurementUnit):
  def this(sized : Sized[MeasurementUnit, T], x : MeasurementUnit, y : MeasurementUnit) = this(sized.value, x, y, sized.width, sized.height)
  
  def axisCoordinate(axis : Axis) : MeasurementUnit =
    axis match
      case Axis.Vertical => y
      case Axis.Horizontal => x
    end match
  end axisCoordinate
  
  def axisValue(axis: Axis) : MeasurementUnit =
    axis match
      case Axis.Vertical => height
      case Axis.Horizontal => width
    end match
  end axisValue
end Placed
