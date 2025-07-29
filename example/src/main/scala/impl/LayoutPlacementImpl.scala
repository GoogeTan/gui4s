package me.katze.gui4s.example
package impl

import api.LayoutPlacementMeta
import place.*

import catnip.syntax.all.given
import cats.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds}
import me.katze.gui4s.layout.rowcolumn.{measureItems, sizeItems}
import me.katze.gui4s.layout.{*, given}

import scala.annotation.experimental

// TODO rename me
@experimental
def containerPlacementCurriedOvergeneralized[
  Place[_] : Monad,
  Widget,
  MeasurementUnit : Numeric,
](
    getBounds: GetBounds[Place, MeasurementUnit],
    setBounds: SetBounds[Place, MeasurementUnit],
    mainAxis : Axis,
    children : List[Place[Sized[MeasurementUnit, Widget]]],
    mainAxisPlacement : (List[MeasurementUnit], AxisBounds[MeasurementUnit]) => Place[List[MeasurementUnit]],
    additionalAxisPlacement : (MeasurementUnit, AxisBounds[MeasurementUnit]) => Place[MeasurementUnit]
) : Place[Sized[MeasurementUnit, List[(Widget, LayoutPlacementMeta[MeasurementUnit])]]] =
  Monad[Place].flatMap2(
    measureItems[
      Place,
      MeasurementUnit,
      Widget,
    ](
      children,
      mainAxis,
      getBounds,
      setBounds,
    ),
    getBounds
  )((sizedItems, bounds) =>
    rowColumnPlace[Place, MeasurementUnit, Widget](
      elements = sizedItems,
      bounds = AxisDependentBounds.fromConstraints(bounds, mainAxis),
      mainAxisPlace = mainAxisPlacement,
      additionalAxisPlace = additionalAxisPlacement,
      zLevel = Numeric[MeasurementUnit].zero
    )
      .map(sizeItems)
      .map(_.mapValue(unpack))
  )
end containerPlacementCurriedOvergeneralized


