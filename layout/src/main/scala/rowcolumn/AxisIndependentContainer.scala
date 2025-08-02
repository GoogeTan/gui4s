package me.katze.gui4s.layout
package rowcolumn

import bound.*

import cats.*
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
  MeasurementUnit : Numeric, 
  Item
](
  items : List[Place[Sized[MeasurementUnit, Item]]],
  mainAxis : Axis,
  getBounds : Place[Bounds[MeasurementUnit]],
  setBounds : Bounds[MeasurementUnit] => Place[Unit],
) : Place[List[Sized[MeasurementUnit, Item]]] =
    measureItemsKeepingBoundsSame(
      items,
      updateBoundsWithSizedItem[Place, MeasurementUnit, Item](f => getBounds.map(f) >>= setBounds)(_, mainAxis),
      getBounds,
      setBounds
    )
end measureItems

def measureItemsKeepingBoundsSame[
  Measure[_] : Monad,
  MeasurementUnit,
  Item
](
  items : List[Measure[Item]],
  updateBounds : Item => Measure[Unit],
  getBounds : Measure[Bounds[MeasurementUnit]],
  setBounds : Bounds[MeasurementUnit] => Measure[Unit],
) : Measure[List[Item]] =
  for
    initial <- getBounds
    measuredItems <- measureItemsDirty(
      updateBounds,
      items
    )
    _ <- setBounds(initial)
  yield measuredItems
end measureItemsKeepingBoundsSame

def measureItemsDirty[Measure[_] : Monad, Item](updateBounds : Item => Measure[Unit], items : List[Measure[Item]]) : Measure[List[Item]] =
  items.foldM(Nil):
    (processedElements, current) =>
        for
          currentItem <- current
          _ <- updateBounds(currentItem)
        yield processedElements :+ currentItem
end measureItemsDirty

def rowColumnPlace[
  Place[_] : Applicative,
  MeasurementUnit : Numeric as measurementUnitsAreNumbers,
  T
](
  elements           : List[Sized[MeasurementUnit, T]],
  bounds             : AxisDependentBounds[MeasurementUnit],
  mainAxisPlace      : MainAxisPlacement[Place, MeasurementUnit],
  additionalAxisPlace: AdditionalAxisPlacement[Place, MeasurementUnit],
  zLevel : MeasurementUnit,
): Place[Sized[MeasurementUnit, List[Placed[MeasurementUnit, T]]]] =
  Applicative[Place].map2(
    mainAxisPlace(elements.map(_.lengthAlong(bounds.axis)), bounds.mainAxis),
    elements.traverse(element => additionalAxisPlace(element.lengthAlongAnother(bounds.axis), bounds.additionalAxis))
  ) {
    case ((mainAxisCoordinateOfEnd, mainAxisElementsCoordinates), additionalAxisElementsPlaced) =>
      val additionalAxisElementsCoordinates = additionalAxisElementsPlaced.map(_.coordinateOfStart)
      val additionalAxisCoordinateOfEnd = additionalAxisElementsPlaced.map(_.coordinateOfEnd).maxOption.getOrElse(measurementUnitsAreNumbers.zero)
      val coordinatesCombined = combineCoordinates(bounds.axis, mainAxisElementsCoordinates, additionalAxisElementsCoordinates, zLevel)
      Sized(
        elements
          .zip(coordinatesCombined)
          .map(new Placed(_, _)),
        new Rect(
          bounds.axis,
          mainAxisCoordinateOfEnd,
          additionalAxisCoordinateOfEnd
        )
      )
  }
end rowColumnPlace

def combineCoordinates[MeasurementUnit](axis : Axis, mainAxis : List[MeasurementUnit], additionalAxis : List[MeasurementUnit], zLevel : MeasurementUnit) : List[Point3d[MeasurementUnit]] =
  if axis == Axis.Vertical then
    additionalAxis.zip(mainAxis).map((x, y) => Point3d(x, y, zLevel))
  else
    mainAxis.zip(additionalAxis).map((x, y) => Point3d(x, y, zLevel))
  end if
end combineCoordinates
