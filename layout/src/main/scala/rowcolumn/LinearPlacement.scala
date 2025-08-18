package me.katze.gui4s.layout
package rowcolumn

import bound.*

import cats.*
import cats.data.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.*
import me.katze.gui4s.geometry.*

def updateBoundsWithSizedItem[Place[_], MeasurementUnit : Numeric, T](
                                                                        updateBounds : (Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) => Place[Unit]
                                                                      )(
                                                                        sized: Sized[MeasurementUnit, T],
                                                                        mainAxis : Axis
                                                                      ) : Place[Unit] =
  updateBounds(_.cutAlong(mainAxis, sized.lengthAlong(mainAxis)))
end updateBoundsWithSizedItem

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
