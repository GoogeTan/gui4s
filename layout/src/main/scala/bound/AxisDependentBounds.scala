package me.katze.gui4s.layout
package bound

import me.katze.gui4s.geometry.Axis

final case class AxisDependentBounds[+T](boundsAlongMainAxis : AxisBounds[T], boundsalongCrossAxis : AxisBounds[T], mainAxis : Axis):
  val bounds : Bounds[T] =
    mainAxis match
      case Axis.Vertical => Bounds(boundsalongCrossAxis, boundsAlongMainAxis)
      case Axis.Horizontal => Bounds(boundsAlongMainAxis, boundsalongCrossAxis)
    end match
  end bounds
end AxisDependentBounds

object AxisDependentBounds:
  def fromBounds[T](bounds: Bounds[T], axis: Axis) : AxisDependentBounds[T] = AxisDependentBounds(
    if axis == Axis.Vertical then bounds.vertical else bounds.horizontal,
    if axis == Axis.Vertical then bounds.horizontal else bounds.vertical,
    axis
  )
  end fromBounds
end AxisDependentBounds
