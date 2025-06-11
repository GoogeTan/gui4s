package me.katze.gui4s.example
package impl

import api.LayoutPlacementMeta
import place.*

import cats.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.bound.{AxisDependentBounds, Bounds}
import me.katze.gui4s.layout.rowcolumn.{rowColumnPlaceSized, weightedRowColumnPlace}
import me.katze.gui4s.layout.{*, given}

type LayoutPlacement[Place[_], MeasurementUnit, PlacementMeta, Widget[_], Axis] =
  [Event] => (
    mainAxis               : Axis,
    children               : List[Place[Widget[Event]]],
    mainAxisStrategy       : MainAxisPlacementStrategy[MeasurementUnit],
    additionalAxisStrategy : AdditionalAxisPlacementStrategy
  ) => Place[List[(Widget[Event], PlacementMeta)]]

type LayoutPlacementGeneralized[Place[_], MeasurementUnit, PlacementMeta, Widget, Axis] =
  (
    mainAxis               : Axis,
    children               :  List[Place[Widget]],
    mainAxisStrategy       :  MainAxisPlacementStrategy[MeasurementUnit],
    additionalAxisStrategy : AdditionalAxisPlacementStrategy
  ) => Place[List[(Widget, PlacementMeta)]]

def containerPlacementCurried[F[+_] : Monad, Widget[_], MeasurementUnit: Fractional](strategyErrors : MainAxisStrategyErrors) :  LayoutPlacement[MeasurableT[F, MeasurementUnit], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  [Event] => (axis, elements, main, additional) => containerPlacementCurriedGeneralized[F, Widget[Event], MeasurementUnit](strategyErrors)(axis, elements, main, additional)
end containerPlacementCurried

// TODO Убрать не оправданное переиспользование кода с весами или обосновать его
def containerPlacementCurriedGeneralized[F[+_] : Monad, Widget, MeasurementUnit: Fractional](strategyErrors : MainAxisStrategyErrors): LayoutPlacementGeneralized[MeasurableT[F, MeasurementUnit], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  (axis, elements, mainAxisStrategy, additionalAxisStrategy) =>
    val rowColumnPlacement = (elements : List[Sized[MeasurementUnit, Widget]], axisDependentBounds : AxisDependentBounds[MeasurementUnit]) => rowColumnPlace(
      elements = elements,
      bounds = axisDependentBounds,
      mainAxisPlace = (elements, bounds) =>
        mainAxisStrategyPlacement[MeasurementUnit](unsafeSizedStrategy(mainAxisStrategy, bounds.max, strategyErrors), elements),
      additionalAxisPlace = (elements, bounds) =>
        additionalAxisStrategyPlacement[MeasurementUnit](additionalAxisStrategy, elements, bounds.maxValueUnsafe),
      zLevel = Fractional[MeasurementUnit].zero
    )
    weightedRowColumnPlace[F, MeasurementUnit, Widget](
      mainAxis = axis,
      elements = elements.map(widget => MaybeWeighted(None, widget)),
      rowColumnPlace = rowColumnPlacement
    ).map(unpack)
end containerPlacementCurriedGeneralized

def containerPlacementCurriedGeneralized[Place[+_] : Monad, Widget, MeasurementUnit: Fractional](
                                                                                                  strategyErrors: MainAxisStrategyErrors,
                                                                                                  getBounds : Place[Bounds[MeasurementUnit]],
                                                                                                  updateBounds : (Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) => Place[Unit]
                                                                                                ): LayoutPlacementGeneralized[Place, MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  (axis, elements, mainAxisStrategy, additionalAxisStrategy) =>
    val rowColumnPlacement = (elements: List[Sized[MeasurementUnit, Widget]]) =>
      getBounds.map:
        bounds =>
          rowColumnPlace(
            elements = elements,
            bounds = AxisDependentBounds.fromConstraints(bounds, axis),
            mainAxisPlace = (elements, bounds) =>
              mainAxisStrategyPlacement[MeasurementUnit](unsafeSizedStrategy(mainAxisStrategy, bounds.max, strategyErrors), elements),
            additionalAxisPlace = (elements, bounds) =>
              additionalAxisStrategyPlacement[MeasurementUnit](additionalAxisStrategy, elements, bounds.maxValueUnsafe),
            zLevel = Fractional[MeasurementUnit].zero
          )
    rowColumnPlaceSized[
      Place,
      MeasurementUnit,
      Widget,
      List[(Widget, LayoutPlacementMeta[MeasurementUnit])]
    ](
      elements, 
      axis,
      updateBounds,
      rowColumnPlacement
    )
end containerPlacementCurriedGeneralized

val ENErrors = MainAxisStrategyErrors(
  spaceBetweenInInfiniteContainer = "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  spaceAroundInInfiniteContainer = "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  spaceCenterInInfiniteContainer = "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  spaceEndInInfiniteContainer = "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

