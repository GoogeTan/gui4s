package me.katze.gui4s.layout
package linear

import scala.math.Numeric.Implicits.infixNumericOps

import cats.{Foldable, Traverse}
import cats.syntax.all.*
import me.katze.gui4s.geometry.*

def placeEnd[MeasurementUnit : Numeric](size : MeasurementUnit, space : MeasurementUnit) : MeasurementUnit = space - size

def placeEndMany[Container[_] : Traverse, MeasurementUnit: Numeric](sizes: Container[MeasurementUnit], space: MeasurementUnit): Container[Rect1dOnPoint1d[MeasurementUnit]] =
  val allSize = sizes.foldLeft(Numeric[MeasurementUnit].zero)(_ + _)
  placeBeginTailrecHelper(sizes, space - allSize)
end placeEndMany

def placeEndManyWithGap[Container[_] : Traverse, MeasurementUnit: Numeric as TF](sizes: Container[MeasurementUnit], space: MeasurementUnit, gap: MeasurementUnit): Container[Rect1dOnPoint1d[MeasurementUnit]] =
  placeEndMany(sizes.map(_ + gap), space).map(_.addLength(-gap).addCoordinateOfTheBeginning(gap))
end placeEndManyWithGap
