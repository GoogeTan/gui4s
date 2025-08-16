package me.katze.gui4s.layout
package linear

import cats.data.NonEmptyList
import cats.{Applicative, SemigroupK, Traverse}
import cats.syntax.all.*

import scala.math.Fractional.Implicits.*

def placeSpaceAround[Container[_] : {Applicative as A, Traverse, SemigroupK}, MeasurementUnit : Fractional](sizes : Container[MeasurementUnit], space : MeasurementUnit) : Container[Rect1dOnPoint1d[MeasurementUnit]] =
  val zero = Fractional[MeasurementUnit].zero.pure[Container]
  val gap = spaceBetween(zero <+> sizes <+> zero, space)
  A.map(placeBeginTailrecHelper(A.map(sizes)(_ + gap), gap))(sized => sized.copy(length = sized.length - gap))
end placeSpaceAround
