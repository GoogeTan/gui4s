package me.katze.gui4s.layout
package bound

import scala.math.Ordered.orderingToOrdered
import scala.math.Numeric.Implicits.*

// TODO Написать, что max=None кодирует отсутствие верхнего предела => бесконечность
final case class AxisBounds[+MeasurementUnit](max: Option[MeasurementUnit]):
  def this(max : MeasurementUnit) =
    this(Some(max))
  end this

  def zero(using N : Numeric[? >: MeasurementUnit]) : Boolean = max.contains(N.zero)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def maxValueUnsafe : MeasurementUnit = 
    max.getOrElse(throw Exception("Infinite size container has infinite size children"))
  end maxValueUnsafe

  def cut[T >: MeasurementUnit](amount : T)(using N: Numeric[T]) : AxisBounds[T] =
    max match
      case Some(value) =>
        if N.compare(value, amount) == 1 then
          AxisBounds(Option(N.zero))
        else
          AxisBounds(Option(N.minus(value, amount)))
      case None => this
    end match
  end cut
end AxisBounds

extension[MeasurementUnit : Numeric](self : AxisBounds[MeasurementUnit])
  def withMaxValue(value: Option[MeasurementUnit]): AxisBounds[MeasurementUnit] =
    AxisBounds(value)
  end withMaxValue
end extension