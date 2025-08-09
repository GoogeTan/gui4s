package me.katze.gui4s.layout
package linear

import cats.data.NonEmptyList
import scala.math.Fractional.Implicits.*

def placeCenter[T : Fractional](size : T, space : T) : T = (space - size) / Numeric[T].fromInt(2)

def placeCenterMany[T : Fractional](sizes : List[T], space : T) : List[Rect1dOnPoint1d[T]] =
  val allSize = sizes.sum
  placeBeginTailrecHelper(sizes, (space - allSize) / Numeric[T].fromInt(2), Nil)
end placeCenterMany

def placeCenterMany[T: Fractional](sizes: NonEmptyList[T], space: T): NonEmptyList[Rect1dOnPoint1d[T]] =
  val allSize = sizes.toList.sum
  placeBeginTailrecHelper(sizes, (space - allSize) / Numeric[T].fromInt(2), Nil)
end placeCenterMany

def placeCenterManyWithGap[T : Fractional as TF](sizes : List[T], space : T, gap : T) : List[Rect1dOnPoint1d[T]] =
  placeCenterMany(sizes.map(_ + gap), space).map(_.addLength(-gap).addCoordinateOfTheBeginning(gap / TF.fromInt(2)))
end placeCenterManyWithGap
