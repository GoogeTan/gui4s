package me.katze.gui4s.layout
package linear

import cats.{Foldable, Traverse}
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.math.Fractional.Implicits.*
import me.katze.gui4s.geometry.*

def placeCenter[T : Fractional](size : T, space : T) : T = (space - size) / Numeric[T].fromInt(2)

def placeCenterMany[Container[_] : Traverse, T: Fractional](sizes: Container[T], space: T): Container[Rect1dOnPoint1d[T]] =
  val allSize = sizes.foldLeft(Numeric[T].zero)(_ + _)
  placeBeginTailrecHelper(sizes, (space - allSize) / Numeric[T].fromInt(2))
end placeCenterMany

def placeCenterManyWithGap[Container[_] : Traverse, T: Fractional as TF](sizes: Container[T], space: T, gap: T): Container[Rect1dOnPoint1d[T]] =
  placeCenterMany(sizes.map(_ + gap), space).map(_.addLength(-gap).addCoordinateOfTheBeginning(gap / TF.fromInt(2)))
end placeCenterManyWithGap
