package me.katze.gui4s.example
package impl

import api.impl.{LayoutPlacementGeneralized, LayoutPlacementMeta}
import place.*

import cats.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}

def containerPlacementCurried[F[+_] : Monad, Widget, MeasurementUnit: Fractional](strategyErrors : MainAxisStrategyErrors): LayoutPlacementGeneralized[MeasurableT[F, MeasurementUnit], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget] =
  (axis, elements, main, additional) =>
    weightedRowColumnPlace[F, MeasurementUnit, Widget](
      axis,
      elements.map(widget => MaybeWeighted(None, widget)),
      rowColumnPlace(_, _,
        (elements, bounds) => mainAxisStrategyPlacement[MeasurementUnit](unsafeSizedStrategy(main, bounds.max, strategyErrors), elements),
        (elements, bounds) => additionalAxisStrategyPlacement[MeasurementUnit](additional, elements, bounds.maxValueUnsafe))
    ).map(unpack)
end containerPlacementCurried

val ENErrors = MainAxisStrategyErrors(
  "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

