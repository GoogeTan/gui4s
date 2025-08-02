package me.katze.gui4s.layout
package linear

import scala.math.Fractional.Implicits.infixFractionalOps

def placeCenter[T : Fractional](size : T, space : T) : T = (space - size) / Numeric[T].fromInt(2)

def placeCenterMany[T : Fractional](sizes : List[T], space : T) : List[SizedElement[T]] =
  val allSize = sizes.sum
  placeBeginTailrecHelper(sizes, (space - allSize) / Numeric[T].fromInt(2), Nil)
end placeCenterMany


def placeCenterManyWithGap[T : Fractional as TF](sizes : List[T], space : T, gap : T) : List[SizedElement[T]] =
  placeCenterMany(sizes.map(_ + gap), space).map(_.addSize(-gap).addBeginCoordinate(gap / TF.fromInt(2)))
end placeCenterManyWithGap
