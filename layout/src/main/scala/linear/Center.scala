package me.katze.gui4s.layout
package linear

import scala.math.Fractional.Implicits.infixFractionalOps

def placeCenter[T : Fractional](size : T, space : T) : T = (space - size) / Numeric[T].fromInt(2)

def placeCenterMany[T : Fractional](sizes : List[T], space : T) : List[T] =
  val allSize = sizes.sum
  placeBeginTailrec(sizes, (space - allSize) / Numeric[T].fromInt(2))
end placeCenterMany
