package gui4s.core.layout
package linear

import scala.math.Fractional.Implicits.*

import cats.Applicative
import cats.SemigroupK
import cats.Traverse
import cats.syntax.all.*

import gui4s.core.geometry.*

//TODO Удалить слишком много тайпклассов у контейнера.
def placeSpaceAround[Collection[_] : {Applicative as A, Traverse, SemigroupK}, MeasurementUnit : Fractional](sizes : Collection[MeasurementUnit], space : MeasurementUnit) : Collection[Rect1dOnPoint1d[MeasurementUnit]] =
  val zero = Fractional[MeasurementUnit].zero.pure[Collection]
  val gap = spaceBetween(zero <+> sizes <+> zero, space)
  A.map(placeBeginTailrecHelper(A.map(sizes)(_ + gap), gap))(sized => sized.copy(length = sized.length - gap))
end placeSpaceAround
