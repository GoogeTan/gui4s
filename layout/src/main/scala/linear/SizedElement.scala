package me.katze.gui4s.layout
package linear

import scala.math.Numeric.Implicits.*

final case class SizedElement[T](size: T, coordinateOfStart: T):
  def coordinateOfEnd(using n : Numeric[T]): T = size + coordinateOfStart
  
  def addBeginCoordinate(using n : Numeric[T])(value : T) : SizedElement[T] =
    SizedElement(size, coordinateOfStart + value)
  end addBeginCoordinate

  def addSize(using n : Numeric[T])(value : T) : SizedElement[T] =
    SizedElement(size + value, coordinateOfStart)
  end addSize
end SizedElement

object SizedElement:
  def empty[T](using N : Numeric[T]) : SizedElement[T] =
    SizedElement(N.zero, N.zero)
  end empty
end SizedElement
