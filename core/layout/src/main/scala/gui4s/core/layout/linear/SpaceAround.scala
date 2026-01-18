package gui4s.core.layout
package linear

import scala.math.Fractional.Implicits._

import cats.Applicative
import cats.SemigroupK
import cats.Traverse
import cats.syntax.all._

import gui4s.core.geometry._

//TODO Удалить слишком много тайпклассов у контейнера.
def placeSpaceAround[Container[_] : {Applicative as A, Traverse, SemigroupK}, MeasurementUnit : Fractional](sizes : Container[MeasurementUnit], space : MeasurementUnit) : Container[Rect1dOnPoint1d[MeasurementUnit]] =
  val zero = Fractional[MeasurementUnit].zero.pure[Container]
  val gap = spaceBetween(zero <+> sizes <+> zero, space)
  A.map(placeBeginTailrecHelper(A.map(sizes)(_ + gap), gap))(sized => sized.copy(length = sized.length - gap))
end placeSpaceAround
