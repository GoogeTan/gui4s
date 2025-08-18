package me.katze.gui4s.layout
package rowcolumn

import bound.{Bounds, GetBounds, SetBounds}
import rowcolumn.measureItems

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
  MeasurementUnit : Numeric as MUN,
](
  getBounds: Place[Bounds[MeasurementUnit]],
  setBounds: SetBounds[Place, MeasurementUnit],
  mainAxis : Axis,
  children : Container[Place[Sized[MeasurementUnit, Widget]]],
  elementsPlacement : ManyElementsPlacementStrategy[Place, Point2d[InfinityOr[MeasurementUnit]], Container, Point2d[MeasurementUnit]],
) : Place[Sized[MeasurementUnit, Container[Placed[MeasurementUnit, Widget]]]] =
  for
    sizedItems <- measureItems[
      Place,
      Container,
      MeasurementUnit,
      Widget,
    ](
      children,
      mainAxis,
      getBounds,
      setBounds,
    )
    bounds <- getBounds
    placedItems <- elementsPlacement(sizedItems.map(_.size.asPoint2d), bounds.asPoint2d)
  yield Sized(
    sizedItems.zip(placedItems.coordinatesOfStarts).map(new Placed(_, _, MUN.zero)),
    new Rect(placedItems.coordinateOfEnd)
  )
end rowColumnLayoutPlacement


