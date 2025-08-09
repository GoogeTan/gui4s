package me.katze.gui4s.layout
package rowcolumn

import cats.*
import cats.syntax.all.*
import catnip.syntax.all.*
import me.katze.gui4s.geometry.Axis
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds, GetBounds, SetBounds}
import me.katze.gui4s.layout.rowcolumn.{measureItems, sizeItems}
import me.katze.gui4s.layout.{*, given}

import scala.annotation.experimental
import scala.math.Numeric.Implicits.*
import scala.math.Fractional.Implicits.*

@experimental
def rowColumnLayoutPlacement[
  Place[_] : Monad,
  Widget,
  MeasurementUnit : Numeric,
](
  getBounds: GetBounds[Place, MeasurementUnit],
  setBounds: SetBounds[Place, MeasurementUnit],
  mainAxis : Axis,
  children : List[Place[Sized[MeasurementUnit, Widget]]],
  mainAxisPlacement : MainAxisPlacement[Place, MeasurementUnit], 
  additionalAxisPlacement : AdditionalAxisPlacement[Place, MeasurementUnit] 
) : Place[Sized[MeasurementUnit, List[Placed[MeasurementUnit, Widget]]]] =
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
      bounds = AxisDependentBounds.fromBounds(bounds, mainAxis),
      mainAxisPlace = mainAxisPlacement,
      additionalAxisPlace = additionalAxisPlacement,
      zLevel = Numeric[MeasurementUnit].zero
    )
  )
end rowColumnLayoutPlacement


