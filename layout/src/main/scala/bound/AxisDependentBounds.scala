package me.katze.gui4s.layout
package bound

final case class AxisDependentBounds[+T : Numeric](mainAxis : AxisBounds[T], additionalAxis : AxisBounds[T], axis : Axis):
  val bounds : Bounds[T] =
    axis match
      case Axis.Vertical => Bounds(additionalAxis, mainAxis)
      case Axis.Horizontal => Bounds(mainAxis, additionalAxis)
    end match
  end bounds

  // TODO Почему-то ругается на эни в интерполяции строки...
  @SuppressWarnings(Array("org.wartremover.warts.Any")) 
  override def toString: String = s"AxisDependentBounds(axis=$axis, mainAxis(min=${mainAxis.min}, max=${mainAxis.max}), additionalAxis(min=${additionalAxis.min}, max=${additionalAxis.max}))"
end AxisDependentBounds

object AxisDependentBounds:
  def fromConstraints[T : Numeric](constraints: Bounds[T], axis: Axis) : AxisDependentBounds[T] = AxisDependentBounds(
    if axis == Axis.Vertical then constraints.vertical else constraints.horizontal,
    if axis == Axis.Vertical then constraints.horizontal else constraints.vertical,
    axis
  )
  end fromConstraints
end AxisDependentBounds
