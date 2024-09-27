package me.katze.gui4s.layout
package bound

import scala.math.Ordered.orderingToOrdered

final case class AxisBounds[+T : Numeric](min: Option[T], max: Option[T]):
  assert(min.zip(max).forall(_ <= _))
  
  def fixed: Boolean = min == max
  def finite : Boolean = max.isDefined
  def zero : Boolean = max.contains(Numeric[T].zero)
  
  
end AxisBounds

extension[T : Numeric](self : AxisBounds[T])
  def withMaxValue(value: Option[T]): AxisBounds[T] =
    AxisBounds(self.min.zip(value).map(Numeric[T].min), value)
  end withMaxValue
end extension