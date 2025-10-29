package gui4s.core.layout
package linear

import cats.syntax.all.*
import cats.{Applicative, SemigroupK, Traverse}
import gui4s.core.geometry.*

import scala.math.Fractional.Implicits.*

//TODO Удалить слишком много тайпклассов у контейнера.
def placeSpaceAround[Container[_] : {Applicative as A, Traverse, SemigroupK}, MeasurementUnit : Fractional](sizes : Container[MeasurementUnit], space : MeasurementUnit) : Container[Rect1dOnPoint1d[MeasurementUnit]] =
  val zero = Fractional[MeasurementUnit].zero.pure[Container]
  val gap = spaceBetween(zero <+> sizes <+> zero, space)
  A.map(placeBeginTailrecHelper(A.map(sizes)(_ + gap), gap))(sized => sized.copy(length = sized.length - gap))
end placeSpaceAround
