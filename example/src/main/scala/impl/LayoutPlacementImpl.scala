package me.katze.gui4s.example
package impl

import api.LayoutPlacementMeta
import place.*

import cats.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.rowcolumn.weightedRowColumnPlace
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{LayoutPlacement, LayoutPlacementGeneralized}

def containerPlacementCurried2[F[+_] : Monad, Widget[_], MeasurementUnit: Fractional](strategyErrors : MainAxisStrategyErrors) :  LayoutPlacement[MeasurableT[F, MeasurementUnit], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  [Event] => (axis, elements, main, additional) => containerPlacementCurried[F, Widget[Event], MeasurementUnit](strategyErrors)(axis, elements, main, additional)
end containerPlacementCurried2

// TODO Убрать не оправданное переиспользование кода с весами или обосновать его
def containerPlacementCurried[F[+_] : Monad, Widget, MeasurementUnit: Fractional](strategyErrors : MainAxisStrategyErrors): LayoutPlacementGeneralized[MeasurableT[F, MeasurementUnit], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
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
  spaceBetweenInInfiniteContainer = "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  spaceAroundInInfiniteContainer = "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  spaceCenterInInfiniteContainer = "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  spaceEndInInfiniteContainer = "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

