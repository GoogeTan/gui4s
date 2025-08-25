package gui4s.core.geometry

import scala.math.Numeric.Implicits.*

final case class Rect1dOnPoint1d[T](length: T, coordinateOfTheBeginning: T):
  def coordinateOfTheEnd(using n : Numeric[T]): T = length + coordinateOfTheBeginning
  
  def addCoordinateOfTheBeginning(using n : Numeric[T])(value : T) : Rect1dOnPoint1d[T] =
    Rect1dOnPoint1d(length, coordinateOfTheBeginning + value)
  end addCoordinateOfTheBeginning

  def addLength(using n : Numeric[T])(value : T) : Rect1dOnPoint1d[T] =
    Rect1dOnPoint1d(length + value, coordinateOfTheBeginning)
  end addLength
end Rect1dOnPoint1d

object Rect1dOnPoint1d:
  def empty[T](using N : Numeric[T]) : Rect1dOnPoint1d[T] =
    Rect1dOnPoint1d(N.zero, N.zero)
  end empty

  def fromStartAndEnd[MeasurementUnit : Numeric](coordinateOfStart : MeasurementUnit, coordinateOfEnd : MeasurementUnit) : Rect1dOnPoint1d[MeasurementUnit] =
    Rect1dOnPoint1d(
      length = coordinateOfEnd - coordinateOfStart,
      coordinateOfTheBeginning = coordinateOfStart
    )
  end fromStartAndEnd
end Rect1dOnPoint1d
