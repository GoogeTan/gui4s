package me.katze.gui4s.layout
package linear

import scala.math.Numeric.Implicits.infixNumericOps

def placeEnd[T : Numeric](size : T, space : T) : T = space - size

def placeEndMany[T: Numeric](sizes : List[T], space : T) : List[T] =
  val allSize = sizes.sum
  placeBeginTailrec(sizes, space - allSize)
end placeEndMany
