package me.katze.gui4s.layout
package bound

import me.katze
import me.katze.gui4s
import me.katze.gui4s.layout

final case class Bounds[+MeasurementUnit](horizontal : AxisBounds[MeasurementUnit], vertical : AxisBounds[MeasurementUnit]):
  def this(width : MeasurementUnit, height : MeasurementUnit)(using Numeric[MeasurementUnit]) =
    this(new AxisBounds(width), new AxisBounds(height))
  end this

  def cutAlong[T >: MeasurementUnit](axis: Axis, amount : T)(using Numeric[T]) : Bounds[T] =
    axis match
      case Axis.Vertical => 
        copy(
          vertical = vertical.cut(amount)
        )
      case Axis.Horizontal =>
        copy(
          horizontal = horizontal.cut(amount)
        )
    end match
  end cutAlong
  
  def along(axis : Axis) : AxisBounds[MeasurementUnit] =
    axis match
      case katze.gui4s.layout.Axis.Vertical => horizontal
      case katze.gui4s.layout.Axis.Horizontal => vertical
    end match
  end along
end Bounds
