package me.katze.gui4s.example
package impl

import api.impl.LayoutPlacement
import place.*

import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.Widget

def containerPlacementCurried[Update[+_, +_], Draw, Recomposition, DownEvent, MeasurementUnit: Fractional](strategyErrors : MainAxisStrategyErrors): LayoutPlacement[Update, Draw, MeasurableT[MeasurementUnit], Recomposition, DownEvent, MeasurementUnit] =
  [Event] => (axis: Axis, elements, main, additional) =>
    weightedRowColumnPlace[MeasurementUnit, Widget[Update, Draw, MeasurableT[MeasurementUnit], Recomposition, Event, DownEvent]](
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

