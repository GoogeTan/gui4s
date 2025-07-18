package me.katze.gui4s.example
package impl

import api.LayoutPlacementMeta
import place.*

import cats.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds, Bounds}
import me.katze.gui4s.layout.rowcolumn.{placeItems, sizeItems}
import me.katze.gui4s.layout.{*, given}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}

import scala.annotation.experimental

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

@experimental
def containerPlacementCurried[
  Place[_] : MonadErrorT[Error],
  Widget[_], 
  MeasurementUnit: Fractional,
  Error
](
  strategyErrors : MainAxisStrategyErrors[Error],
  getBounds : GetBounds[Place, MeasurementUnit],
  setBounds : SetBounds[Place, MeasurementUnit],
) :  LayoutPlacement[[Value] =>> Place[Sized[MeasurementUnit, Value]], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  [Event] => (axis, elements, main, additional) => 
    containerPlacementCurriedGeneralized[Place, Widget[Event], MeasurementUnit, Error](strategyErrors, getBounds, setBounds)(axis, elements, main, additional)
end containerPlacementCurried

@experimental
def containerPlacementCurriedGeneralized[
  Place[_] : MonadErrorT[Error], 
  Widget, 
  MeasurementUnit: Fractional, 
  Error
](
  strategyErrors: MainAxisStrategyErrors[Error],
  getBounds : GetBounds[Place, MeasurementUnit],
  setBounds : SetBounds[Place, MeasurementUnit],
): LayoutPlacementGeneralized[[Value] =>> Place[Sized[MeasurementUnit, Value]], MeasurementUnit, LayoutPlacementMeta[MeasurementUnit], Widget, Axis] =
  (axis, elements, mainAxisStrategy, additionalAxisStrategy) =>
    val rowColumnPlacement = (elements: List[Sized[MeasurementUnit, Widget]], bounds : Bounds[MeasurementUnit]) =>
      unsafeSizedStrategy(mainAxisStrategy, bounds.along(axis).max, strategyErrors).map:
        sizedStrategy =>
          sizeItems(
            rowColumnPlace(
              elements = elements,
              bounds = AxisDependentBounds.fromConstraints(bounds, axis),
              mainAxisPlace = (elements, bounds) =>
                mainAxisStrategyPlacement[MeasurementUnit](sizedStrategy, elements),
              additionalAxisPlace = (elements, bounds) => 
                additionalAxisStrategyPlacement[MeasurementUnit](additionalAxisStrategy, elements, bounds.maxValueUnsafe),
              zLevel = Fractional[MeasurementUnit].zero
            )
          ).mapValue(unpack)
          
    placeItems[
      Place,
      MeasurementUnit,
      Widget,
      Place[Sized[MeasurementUnit, List[(Widget, LayoutPlacementMeta[MeasurementUnit])]]]
    ](
      elements, 
      axis,
      getBounds,
      setBounds,
      rowColumnPlacement
    ).flatten
end containerPlacementCurriedGeneralized

val ENErrors = MainAxisStrategyErrors(
  attemptedToPlaceElementsWithStrategyCenterInInfiniteContainer = "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  attemptedToPlaceElementsWithStrategyEndInInfiniteContainer = "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  attemptedToPlaceElementsWithStrategySpaceAroundInInfiniteContainer = "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  attemptedToPlaceElementsWithStrategySpaceBetweenInInfiniteContainer = "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

