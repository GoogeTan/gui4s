package me.katze.gui4s.layout
package rowcolumn

import bound.*

import cats.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.katze.gui4s.geometry.*

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

// TODO отрефакторить это. И сделать норм имена
def rowColumnPlace[
  Place[_] : Applicative,
  Container[_] : {Traverse, Applicative as A},
  MeasurementUnit : Numeric as measurementUnitsAreNumbers,
  T
](
  elements           : Container[Sized[MeasurementUnit, T]],
  bounds             : AxisDependentBounds[MeasurementUnit],
  mainAxisPlace      : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit],
  additionalAxisPlace: OneElementPlacementStrategy[Place, MeasurementUnit],
  zAxisPlace : OneElementPlacementStrategy[Place, MeasurementUnit],
  zip : [A, B] => (Container[A], Container[B]) => Container[(A, B)]
): Place[Sized[MeasurementUnit, Container[Placed[MeasurementUnit, T]]]] =
  Applicative[Place].map3(
    mainAxisPlace(A.map(elements)(_.lengthAlong(bounds.mainAxis)), bounds.boundsAlongMainAxis),
    elements.traverse(element => additionalAxisPlace(element.lengthAlongAnother(bounds.mainAxis), bounds.boundsalongCrossAxis)),
    elements.traverse(element => zAxisPlace(element.lengthAlongAnother(bounds.mainAxis), bounds.boundsalongCrossAxis)),
  ) {
    case ((mainAxisCoordinateOfEnd, mainAxisElementsCoordinates), additionalAxisElementsPlaced, zAxisPlaced) =>
      val additionalAxisElementsCoordinates = A.map(additionalAxisElementsPlaced)(_.coordinateOfTheBeginning)
      val zAxisElementsCoordinates = A.map(zAxisPlaced)(_.coordinateOfTheBeginning)
      val additionalAxisCoordinateOfEnd = A.map(additionalAxisElementsPlaced)(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(measurementUnitsAreNumbers.zero)
      val coordinatesCombined = combineCoordinates(bounds.mainAxis, mainAxisElementsCoordinates, additionalAxisElementsCoordinates, zAxisElementsCoordinates, zip)
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
                                                                          zAxis : Container[MeasurementUnit],
                                                                          zip : [A, B] => (Container[A], Container[B]) => Container[(A, B)]
                                                                        ) : Container[Point3d[MeasurementUnit]] =
  if axis == Axis.Vertical then
    A.map(
        zip(zip(additionalAxis, mainAxis), zAxis)
    )((xy, z) => Point3d(xy._1, xy._2, z))
  else
    A.map(
      zip(zip(mainAxis, additionalAxis), zAxis)
    )((xy, z) => Point3d(xy._1, xy._2, z))
  end if
end combineCoordinates
