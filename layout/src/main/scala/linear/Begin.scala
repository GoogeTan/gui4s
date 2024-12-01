package me.katze.gui4s.layout
package linear

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.{*, given}

def placeBegin[T : Numeric] : T = Numeric[T].zero

def placeBeginMany[T : Numeric](sizes : List[T]) : List[SizedElement[T]] =
  placeBeginTailrecHelper(sizes, Numeric[T].zero, Nil)
end placeBeginMany
  
@tailrec
def placeBeginTailrecHelper[T : Numeric](sizes: List[T], initialGap: T, result: List[SizedElement[T]]): List[SizedElement[T]] =
  sizes match
    case size :: otherSizes =>
      placeBeginTailrecHelper(otherSizes, initialGap + size, SizedElement(size, initialGap) :: result)
    case Nil =>
      result.reverse
  end match
end placeBeginTailrecHelper


