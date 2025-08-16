package me.katze.gui4s.layout
package rowcolumn

import bound.*

import cats.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.katze.gui4s.geometry.*

import scala.language.experimental.namedTypeArguments

def updateBoundsWithSizedItem[Place[_], MeasurementUnit : Numeric, T](
                                                                        updateBounds : (Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) => Place[Unit]
                                                                      )(
                                                                        sized: Sized[MeasurementUnit, T],
                                                                        mainAxis : Axis
                                                                      ) : Place[Unit] =
  updateBounds(_.cutAlong(mainAxis, sized.lengthAlong(mainAxis)))
end updateBoundsWithSizedItem

def sizeItems[MeasurementUnit : Numeric, T](items : List[Placed[MeasurementUnit, T]]) : Sized[MeasurementUnit, List[Placed[MeasurementUnit, T]]] =
  val width = items.map(_.endX).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
  val height = items.map(_.endY).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
  new Sized(items, width, height)
end sizeItems

def measureItems[
  Place[_] : Monad,
  Container[_] : Traverse,
  MeasurementUnit : Numeric, 
  Item
](
  items : Container[Place[Sized[MeasurementUnit, Item]]],
  mainAxis : Axis,
  getBounds : Place[Bounds[MeasurementUnit]],
  setBounds : Bounds[MeasurementUnit] => Place[Unit],
) : Place[Container[Sized[MeasurementUnit, Item]]] =
    measureItemsKeepingBoundsSame(
      items,
      updateBoundsWithSizedItem[Place, MeasurementUnit, Item](f => getBounds.map(f) >>= setBounds)(_, mainAxis),
      getBounds,
      setBounds
    )
end measureItems

def measureItemsKeepingBoundsSame[
  Measure[_] : Monad,
  Container[_] : Traverse,
  MeasurementUnit,
  Item
](
  items : Container[Measure[Item]],
  updateBounds : Item => Measure[Unit],
  getBounds : Measure[Bounds[MeasurementUnit]],
  setBounds : Bounds[MeasurementUnit] => Measure[Unit],
) : Measure[Container[Item]] =
  for
    initial <- getBounds
    measuredItems <- measureItemsDirty(
      updateBounds,
      items
    )
    _ <- setBounds(initial)
  yield measuredItems
end measureItemsKeepingBoundsSame

def measureItemsDirty[Measure[_] : Monad, Container[_] : Traverse, Item](updateBounds : Item => Measure[Unit], items : Container[Measure[Item]]) : Measure[Container[Item]] =
  items.traverse(current =>
    for
      currentItem <- current
      _ <- updateBounds(currentItem)
    yield  currentItem
  )
end measureItemsDirty

def rowColumnPlace[
  Place[_] : Applicative,
  Container[_] : {Traverse, Applicative as A},
  MeasurementUnit : Numeric as measurementUnitsAreNumbers,
  T
](
  elements           : Container[Sized[MeasurementUnit, T]],
  bounds             : AxisDependentBounds[MeasurementUnit],
  mainAxisPlace      : MainAxisPlacement[Place, Container, MeasurementUnit],
  additionalAxisPlace: AdditionalAxisPlacement[Place, MeasurementUnit],
  zLevel : MeasurementUnit,
  zip : [A, B] => (Container[A], Container[B]) => Container[(A, B)]
): Place[Sized[MeasurementUnit, Container[Placed[MeasurementUnit, T]]]] =
  Applicative[Place].map2(
    mainAxisPlace(A.map(elements)(_.lengthAlong(bounds.mainAxis)), bounds.boundsAlongMainAxis),
    elements.traverse(element => additionalAxisPlace(element.lengthAlongAnother(bounds.mainAxis), bounds.boundsalongCrossAxis))
  ) {
    case ((mainAxisCoordinateOfEnd, mainAxisElementsCoordinates), additionalAxisElementsPlaced) =>
      val additionalAxisElementsCoordinates = A.map(additionalAxisElementsPlaced)(_.coordinateOfTheBeginning)
      val additionalAxisCoordinateOfEnd = A.map(additionalAxisElementsPlaced)(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(measurementUnitsAreNumbers.zero)
      val coordinatesCombined = A.map(combineCoordinates(bounds.mainAxis, mainAxisElementsCoordinates, additionalAxisElementsCoordinates, zip[MeasurementUnit, MeasurementUnit]))(new Point3d(_, zLevel))
      Sized(
        A.map(zip(elements, coordinatesCombined))((element, coordinates) => new Placed(element, coordinates)),
        new Rect(
          bounds.mainAxis,
          mainAxisCoordinateOfEnd,
          additionalAxisCoordinateOfEnd
        )
      )
  }
end rowColumnPlace

def combineCoordinates[Container[_] : Applicative as A, MeasurementUnit](
                                                                          axis : Axis,
                                                                          mainAxis : Container[MeasurementUnit],
                                                                          additionalAxis : Container[MeasurementUnit],
                                                                          zip : (Container[MeasurementUnit], Container[MeasurementUnit]) => Container[(MeasurementUnit, MeasurementUnit)]
                                                                        ) : Container[Point2d[MeasurementUnit]] =
  if axis == Axis.Vertical then
    A.map(
        zip(additionalAxis, mainAxis)
    )((x, y) => Point2d(x, y))
  else
    A.map(
      zip(mainAxis, additionalAxis)
    )((x, y) => Point2d(x, y))
  end if
end combineCoordinates
