package me.katze.gui4s.layout
package linear

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.*

def placeBegin[T : Numeric] : T = Numeric[T].zero

def placeBeginMany[T : Numeric](sizes : List[T]) : List[Rect1dOnPoint1d[T]] =
  placeBeginTailrecHelper(sizes, Numeric[T].zero, Nil)
end placeBeginMany

def placeBeginManyWithGap[MeasurementUnit : Numeric](sizes : List[MeasurementUnit], gap : MeasurementUnit) : List[Rect1dOnPoint1d[MeasurementUnit]] =
  placeBeginMany(sizes.map(_ + gap)).map(_.addLength(-gap))
end placeBeginManyWithGap

@tailrec
def placeBeginTailrecHelper[
  MeasurementUnit: Numeric
](
  itemLengths: NonEmptyList[MeasurementUnit],
  placeStartingFrom: MeasurementUnit,
  alreadyPlacedItemsInReverseOrder: List[Rect1dOnPoint1d[MeasurementUnit]]
): NonEmptyList[Rect1dOnPoint1d[MeasurementUnit]] =
  itemLengths match
    case NonEmptyList(size, nextItemToPlace :: itemsLeftToPlace) =>
      placeBeginTailrecHelper(
        itemLengths = NonEmptyList(nextItemToPlace, itemsLeftToPlace),
        placeStartingFrom = placeStartingFrom + size,
        alreadyPlacedItemsInReverseOrder = Rect1dOnPoint1d(size, placeStartingFrom) :: alreadyPlacedItemsInReverseOrder
      )
    case NonEmptyList(size, Nil) =>
      NonEmptyList(Rect1dOnPoint1d(size, placeStartingFrom), alreadyPlacedItemsInReverseOrder).reverse
  end match
end placeBeginTailrecHelper

@tailrec
def placeBeginTailrecHelper[
  MeasurementUnit: Numeric
](
  itemLengths: List[MeasurementUnit],
  placeStartingFrom: MeasurementUnit,
  alreadyPlacedItemsInReverseOrder: List[Rect1dOnPoint1d[MeasurementUnit]]
): List[Rect1dOnPoint1d[MeasurementUnit]] =
  itemLengths match
    case size :: itemsLeftToPlace =>
      placeBeginTailrecHelper(
        itemLengths = itemsLeftToPlace,
        placeStartingFrom = placeStartingFrom + size,
        alreadyPlacedItemsInReverseOrder = Rect1dOnPoint1d(size, placeStartingFrom) :: alreadyPlacedItemsInReverseOrder
      )
    case Nil =>
      alreadyPlacedItemsInReverseOrder.reverse
  end match
end placeBeginTailrecHelper
