package me.katze.gui4s.layout
package bound

import scala.math.Ordered.orderingToOrdered

// TODO Написать, что max=None кодирует отсутствие верхнего предела => бесконечность
final case class AxisBounds[+MeasurementUnit : Numeric](min: Option[MeasurementUnit], max: Option[MeasurementUnit]):
  def this(max : MeasurementUnit) =
    this(None, Some(max))
  end this
  
  assert(min.zip(max).forall(_ <= _), "Maximum must be more them minimum")
  
  def fixed: Boolean = min == max
  def finite : Boolean = max.isDefined
  def zero : Boolean = max.contains(Numeric[MeasurementUnit].zero)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def maxValueUnsafe : MeasurementUnit = 
    max.getOrElse(throw Exception("Infinite size container has infinite size children"))
  end maxValueUnsafe
end AxisBounds

extension[MeasurementUnit : Numeric](self : AxisBounds[MeasurementUnit])
  def withMaxValue(value: Option[MeasurementUnit]): AxisBounds[MeasurementUnit] =
    AxisBounds(self.min.zip(value).map(Numeric[MeasurementUnit].min), value)
  end withMaxValue
end extension