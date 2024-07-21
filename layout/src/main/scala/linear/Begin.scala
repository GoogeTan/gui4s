package me.katze.gui4s.layout
package linear

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.{*, given}

def placeBegin[T : Numeric] : T = Numeric[T].zero

def placeBeginMany[T : Numeric](sizes : List[T]) : List[T] =
  placeBeginTailrec(sizes, Numeric[T].zero)
end placeBeginMany


private[linear] def placeBeginTailrec[T : Numeric](sizes: List[T], initialGap: T): List[T] =
  placeBeginTailrecHelper(sizes, initialGap, Nil).reverse
end placeBeginTailrec
  
@tailrec
private def placeBeginTailrecHelper[T : Numeric](sizes: List[T], initialGap: T, result: List[T]): List[T] =
  sizes match
    case size :: otherSizes =>
      placeBeginTailrecHelper(otherSizes, initialGap + size, initialGap :: result)
    case Nil =>
      result
  end match
end placeBeginTailrecHelper


