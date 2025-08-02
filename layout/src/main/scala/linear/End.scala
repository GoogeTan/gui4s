package me.katze.gui4s.layout
package linear

import scala.math.Numeric.Implicits.infixNumericOps

def placeEnd[T : Numeric](size : T, space : T) : T = space - size

def placeEndMany[T: Numeric](sizes : List[T], space : T) : List[SizedElement[T]] =
  val allSize = sizes.sum
  placeBeginTailrecHelper(sizes, space - allSize, Nil)
end placeEndMany

def placeEndManyWithGap[T : Numeric as TF](sizes : List[T], space : T, gap : T) : List[SizedElement[T]] =
  placeEndMany(sizes.map(_ + gap), space).map(_.addSize(-gap).addBeginCoordinate(gap))
end placeEndManyWithGap
