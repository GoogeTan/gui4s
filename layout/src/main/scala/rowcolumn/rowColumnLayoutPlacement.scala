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
  Container[_] : {Applicative, Traverse},
  Widget,
  MeasurementUnit : Numeric,
](
  getBounds: GetBounds[Place, MeasurementUnit],
  setBounds: SetBounds[Place, MeasurementUnit],
  mainAxis : Axis,
  children : Container[Place[Sized[MeasurementUnit, Widget]]],
  mainAxisPlacement : MainAxisPlacement[Place, Container, MeasurementUnit], 
  additionalAxisPlacement : AdditionalAxisPlacement[Place, MeasurementUnit],
  zip : [A, B] => (Container[A], Container[B]) => Container[(A, B)]
) : Place[Sized[MeasurementUnit, Container[Placed[MeasurementUnit, Widget]]]] =
  Monad[Place].flatMap2(
    measureItems[
      Place,
      Container,
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
    rowColumnPlace[Place, Container, MeasurementUnit, Widget](
      elements = sizedItems,
      bounds = AxisDependentBounds.fromBounds(bounds, mainAxis),
      mainAxisPlace = mainAxisPlacement,
      additionalAxisPlace = additionalAxisPlacement,
      zLevel = Numeric[MeasurementUnit].zero,
      zip = zip
    )
  )
end rowColumnLayoutPlacement


