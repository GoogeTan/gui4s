package me.katze.gui4s.layout
package linear

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.*

def placeBegin[T : Numeric] : T = Numeric[T].zero

def placeBeginMany[T : Numeric](sizes : List[T]) : List[SizedElement[T]] =
  placeBeginTailrecHelper(sizes, Numeric[T].zero, Nil)
end placeBeginMany

def placeBeginManyWithGap[T : Numeric](sizes : List[T], gap : T) : List[SizedElement[T]] =
  placeBeginMany(sizes.map(_ + gap)).map(_.addSize(-gap))
end placeBeginManyWithGap
  
@tailrec
def placeBeginTailrecHelper[T : Numeric](sizes: List[T], initialGap: T, result: List[SizedElement[T]]): List[SizedElement[T]] =
  sizes match
    case size :: otherSizes =>
      placeBeginTailrecHelper(otherSizes, initialGap + size, SizedElement(size, initialGap) :: result)
    case Nil =>
      result.reverse
  end match
end placeBeginTailrecHelper


