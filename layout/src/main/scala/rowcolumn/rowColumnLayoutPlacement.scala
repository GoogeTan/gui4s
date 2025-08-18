package me.katze.gui4s.layout
package rowcolumn

import catnip.Set.*
import catnip.Zip
import catnip.Zip.zip
import cats.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.{Axis, InfinityOr, Point2d, Rect}
import me.katze.gui4s.layout.{*, given}

def rowColumnLayoutPlacement[
  Place[_] : Monad,
  Container[_] : {Traverse, Zip},
  Widget,
  BoundUnit,
  MeasurementUnit : Numeric as MUN,
](
  getBounds: Place[Rect[BoundUnit]],
  setBounds: Rect[BoundUnit] => Place[Unit],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  mainAxis : Axis,
  children : Container[Place[Sized[MeasurementUnit, Widget]]],
  elementsPlacement : ManyElementsPlacementStrategy[Place, Point2d[BoundUnit], Container, Point2d[MeasurementUnit]],
) : Place[Sized[MeasurementUnit, Container[Placed[MeasurementUnit, Widget]]]] =
  for
    initialBounds <- getBounds
    sizedItems <- measureItemsDirty[Place, Container, Sized[MeasurementUnit, Widget]](
      item => update(getBounds, setBounds)(_.mapAlong(mainAxis, cut(_, item.lengthAlong(mainAxis)))),
      children,
    )
    _ <- setBounds(initialBounds)
    placedItems <- elementsPlacement(sizedItems.map(_.size.asPoint2d), initialBounds.asPoint2d)
  yield Sized(
    sizedItems.zip(placedItems.coordinatesOfStarts).map(new Placed(_, _, MUN.zero)),
    new Rect(placedItems.coordinateOfEnd)
  )
end rowColumnLayoutPlacement

def measureItemsDirty[Measure[_] : Monad, Container[_] : Traverse, Item](updateBounds : Item => Measure[Unit], items : Container[Measure[Item]]) : Measure[Container[Item]] =
  items.traverse(current =>
    for
      currentItem <- current
      _ <- updateBounds(currentItem)
    yield  currentItem
  )
end measureItemsDirty
