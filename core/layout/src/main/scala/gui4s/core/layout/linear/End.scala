package gui4s.core.layout
package linear

import scala.math.Numeric.Implicits.infixNumericOps

import cats.Traverse
import cats.syntax.all._

import gui4s.core.geometry._

def placeEnd[MeasurementUnit : Numeric](size : MeasurementUnit, space : MeasurementUnit) : MeasurementUnit = space - size

def placeEndMany[Collection[_] : Traverse, MeasurementUnit: Numeric](sizes: Collection[MeasurementUnit], space: MeasurementUnit): Collection[Rect1dOnPoint1d[MeasurementUnit]] =
  val allSize = sizes.foldLeft(Numeric[MeasurementUnit].zero)(_ + _)
  placeBeginTailrecHelper(sizes, space - allSize)
end placeEndMany

def placeEndManyWithGap[Collection[_] : Traverse, MeasurementUnit: Numeric as TF](sizes: Collection[MeasurementUnit], space: MeasurementUnit, gap: MeasurementUnit): Collection[Rect1dOnPoint1d[MeasurementUnit]] =
  placeEndMany(sizes.map(_ + gap), space).map(_.addLength(-gap).addCoordinateOfTheBeginning(gap))
end placeEndManyWithGap
