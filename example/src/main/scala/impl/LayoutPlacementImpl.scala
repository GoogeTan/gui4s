package me.katze.gui4s.example
package impl

import api.LayoutPlacementMeta
import place.*

import cats.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.bound.{AxisDependentBounds, Bounds}
import me.katze.gui4s.layout.rowcolumn.{placeItems, sizeItems}
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

def containerPlacementCurried[Place[_] : Monad, Widget[_], MeasurementUnit: Fractional](
                                                                                          strategyErrors : MainAxisStrategyErrors,
                                                                                          getBounds : Place[Bounds[MeasurementUnit]],
                                                                                          setBounds : Bounds[MeasurementUnit] => Place[Unit],
                                                                                        ) :  LayoutPlacement[[Value] =>> Place[Sized[MeasurementUnit, Value]], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  [Event] => (axis, elements, main, additional) => 
    containerPlacementCurriedGeneralized[Place, Widget[Event], MeasurementUnit](strategyErrors, getBounds, setBounds)(axis, elements, main, additional)
end containerPlacementCurried

def containerPlacementCurriedGeneralized[Place[_] : Monad, Widget, MeasurementUnit: Fractional](
                                                                                                  strategyErrors: MainAxisStrategyErrors,
                                                                                                  getBounds : Place[Bounds[MeasurementUnit]],
                                                                                                  setBounds : Bounds[MeasurementUnit] => Place[Unit],
                                                                                                ): LayoutPlacementGeneralized[[Value] =>> Place[Sized[MeasurementUnit, Value]], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  (axis, elements, mainAxisStrategy, additionalAxisStrategy) =>
    val rowColumnPlacement = (elements: List[Sized[MeasurementUnit, Widget]], bounds : Bounds[MeasurementUnit]) =>
      sizeItems(
        rowColumnPlace(
          elements = elements,
          bounds = AxisDependentBounds.fromConstraints(bounds, axis),
          mainAxisPlace = (elements, bounds) =>
            mainAxisStrategyPlacement[MeasurementUnit](unsafeSizedStrategy(mainAxisStrategy, bounds.max, strategyErrors), elements),
          additionalAxisPlace = (elements, bounds) => 
            additionalAxisStrategyPlacement[MeasurementUnit](additionalAxisStrategy, elements, bounds.maxValueUnsafe),
          zLevel = Fractional[MeasurementUnit].zero
        )
      ).mapValue(unpack)
          
    placeItems[
      Place,
      MeasurementUnit,
      Widget,
      Sized[MeasurementUnit, List[(Widget, LayoutPlacementMeta[MeasurementUnit])]]
    ](
      elements, 
      axis,
      getBounds,
      setBounds,
      rowColumnPlacement
    )
end containerPlacementCurriedGeneralized

val ENErrors = MainAxisStrategyErrors(
  spaceBetweenInInfiniteContainer = "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  spaceAroundInInfiniteContainer = "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  spaceCenterInInfiniteContainer = "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  spaceEndInInfiniteContainer = "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

