package me.katze.gui4s.layout
package bound

import scala.math.Ordered.orderingToOrdered

// TODO Написать, что max=None кодирует отсутсвие верхнего предела => бесконечность
final case class AxisBounds[+T : Numeric](min: Option[T], max: Option[T]):
  assert(min.zip(max).forall(_ <= _), "Maximum must be more them minimum")
  
  def fixed: Boolean = min == max
  def finite : Boolean = max.isDefined
  def zero : Boolean = max.contains(Numeric[T].zero)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def maxValueUnsafe : T = 
    max.getOrElse(throw Exception("Infinite size container has infinite size children"))
  end maxValueUnsafe
end AxisBounds

extension[T : Numeric](self : AxisBounds[T])
  def withMaxValue(value: Option[T]): AxisBounds[T] =
    AxisBounds(self.min.zip(value).map(Numeric[T].min), value)
  end withMaxValue
end extension