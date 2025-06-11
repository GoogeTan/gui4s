package me.katze.gui4s.layout
package bound

// TODO Переименовать axis в mainAxis и два других поля во что-то более понятное
final case class AxisDependentBounds[+T](mainAxis : AxisBounds[T], additionalAxis : AxisBounds[T], axis : Axis):
  val bounds : Bounds[T] =
    axis match
      case Axis.Vertical => Bounds(additionalAxis, mainAxis)
      case Axis.Horizontal => Bounds(mainAxis, additionalAxis)
    end match
  end bounds
end AxisDependentBounds

object AxisDependentBounds:
  def fromConstraints[T](constraints: Bounds[T], axis: Axis) : AxisDependentBounds[T] = AxisDependentBounds(
    if axis == Axis.Vertical then constraints.vertical else constraints.horizontal,
    if axis == Axis.Vertical then constraints.horizontal else constraints.vertical,
    axis
  )
  end fromConstraints
end AxisDependentBounds
