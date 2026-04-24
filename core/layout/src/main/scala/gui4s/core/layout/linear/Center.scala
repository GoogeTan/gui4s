package gui4s.core.layout
package linear

import scala.math.Fractional.Implicits.*

import cats.Traverse
import cats.syntax.all.*

import gui4s.core.geometry.*

def placeCenter[T : Fractional](size : T, space : T) : T = (space - size) / Numeric[T].fromInt(2)

def placeCenterMany[Collection[_] : Traverse, T: Fractional](sizes: Collection[T], space: T): Collection[Rect1dOnPoint1d[T]] =
  val allSize = sizes.foldLeft(Numeric[T].zero)(_ + _)
  placeBeginTailrecHelper(sizes, (space - allSize) / Numeric[T].fromInt(2))
end placeCenterMany

def placeCenterManyWithGap[Collection[_] : Traverse, T: Fractional as TF](sizes: Collection[T], space: T, gap: T): Collection[Rect1dOnPoint1d[T]] =
  placeCenterMany(sizes.map(_ + gap), space).map(_.addLength(-gap).addCoordinateOfTheBeginning(gap / TF.fromInt(2)))
end placeCenterManyWithGap
