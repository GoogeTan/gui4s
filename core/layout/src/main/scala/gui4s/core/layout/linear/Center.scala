package gui4s.core.layout
package linear

import scala.math.Fractional.Implicits._

import cats.Traverse
import cats.syntax.all._

import gui4s.core.geometry._

def placeCenter[T : Fractional](size : T, space : T) : T = (space - size) / Numeric[T].fromInt(2)

def placeCenterMany[Container[_] : Traverse, T: Fractional](sizes: Container[T], space: T): Container[Rect1dOnPoint1d[T]] =
  val allSize = sizes.foldLeft(Numeric[T].zero)(_ + _)
  placeBeginTailrecHelper(sizes, (space - allSize) / Numeric[T].fromInt(2))
end placeCenterMany

def placeCenterManyWithGap[Container[_] : Traverse, T: Fractional as TF](sizes: Container[T], space: T, gap: T): Container[Rect1dOnPoint1d[T]] =
  placeCenterMany(sizes.map(_ + gap), space).map(_.addLength(-gap).addCoordinateOfTheBeginning(gap / TF.fromInt(2)))
end placeCenterManyWithGap
