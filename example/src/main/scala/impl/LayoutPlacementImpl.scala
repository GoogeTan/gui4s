package me.katze.gui4s.example
package impl

import api.impl.LayoutPlacement
import place.{additionalAxisStrategyPlacement, mainAxisStrategyPlacement, rowColumnPlace, unpack}

import me.katze.gui4s.widget.Widget
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import cats.syntax.all.*

def containerPlacementCurried[Update[+_, +_], Draw, Recomposition, DownEvent, MU: Fractional]: LayoutPlacement[Update, Draw, MeasurableT[MU], Recomposition, DownEvent, MU] =
  [Event] => (axis: Axis, elements, main, additional) =>
    weightedRowColumnPlace[MU, Widget[Update, Draw, MeasurableT[MU], Recomposition, Event, DownEvent]](
      axis,
      elements.map(widget => MaybeWeighted(None, widget)),
      rowColumnPlace(_, _,
        (elements, bounds) => mainAxisStrategyPlacement[MU](main, elements, bounds.maxValueUnsafe),
        (elements, bounds) => additionalAxisStrategyPlacement[MU](additional, elements, bounds.maxValueUnsafe))
    ).map(unpack)
end containerPlacementCurried
