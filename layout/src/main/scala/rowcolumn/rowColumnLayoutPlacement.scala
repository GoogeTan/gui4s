package me.katze.gui4s.layout
package rowcolumn

import bound.{GetBounds, SetBounds}
import rowcolumn.measureItems

import cats.*
import me.katze.gui4s.geometry.Axis
import me.katze.gui4s.layout.{*, given}

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
  mainAxisPlacement : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit],
  additionalAxisPlacement : OneElementPlacementStrategy[Place, MeasurementUnit],
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
      mainAxis = mainAxis,
      bounds = bounds,
      mainAxisPlace = mainAxisPlacement,
      additionalAxisPlace = additionalAxisPlacement,
      zLevel = Numeric[MeasurementUnit].zero,
      zip = zip
    )
  )
end rowColumnLayoutPlacement


