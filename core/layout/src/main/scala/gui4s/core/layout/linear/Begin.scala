package gui4s.core.layout
package linear

import scala.math.Numeric.Implicits._

import cats.Traverse
import cats.syntax.all._

import gui4s.core.geometry._

def placeBegin[MeasurementUnit : Numeric] : MeasurementUnit = Numeric[MeasurementUnit].zero

def placeBeginMany[Collection[_] : Traverse, MeasurementUnit : Numeric](sizes : Collection[MeasurementUnit]) : Collection[Rect1dOnPoint1d[MeasurementUnit]] =
  placeBeginTailrecHelper(sizes, Numeric[MeasurementUnit].zero)
end placeBeginMany

def placeBeginManyWithGap[Collection[_] : Traverse, MeasurementUnit : Numeric](sizes : Collection[MeasurementUnit], gap : MeasurementUnit) : Collection[Rect1dOnPoint1d[MeasurementUnit]] =
  placeBeginMany(sizes.map(_ + gap)).map(_.addLength(-gap))
end placeBeginManyWithGap

def placeBeginTailrecHelper[
  Collection[_] : Traverse,
  MeasurementUnit: Numeric
](
  itemLengths: Collection[MeasurementUnit],
  placeStartingFrom: MeasurementUnit
): Collection[Rect1dOnPoint1d[MeasurementUnit]] =
  itemLengths.mapAccumulate(placeStartingFrom)((currentStart, currentItem) =>
    (
      currentStart + currentItem,
      Rect1dOnPoint1d(currentItem, currentStart)
    )
  )._2
end placeBeginTailrecHelper
