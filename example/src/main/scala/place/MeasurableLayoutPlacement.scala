package me.katze.gui4s.example
package place

import api.LayoutPlacementMeta

import cats.*
import cats.syntax.all.*
import catnip.syntax.all.{*, given}
import me.katze.gui4s.geometry.{Axis, Point3d}
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds}
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.{Placed, Sized}
import me.katze.gui4s.widget.library.*

import scala.math.Fractional.Implicits.*

def mainAxisStrategyPlacement[
  Place[_] : ApplicativeErrorT[PlacementError],
  PlacementError,
  MeasurementUnit : Fractional as F
](
  strategy: MainAxisPlacementStrategy[MeasurementUnit],
  elements: List[MeasurementUnit],
  bounds : AxisBounds[MeasurementUnit],
  errors : ElementPlacementInInfiniteContainerAttemptError[PlacementError]
): Place[List[MeasurementUnit]] =
  strategy match
    case MainAxisPlacementStrategy.Begin(gap) =>
      placeBeginMany(elements.map(_ + gap))
        .map(_.coordinateOfStart)
        .pure[Place]
    case MainAxisPlacementStrategy.Center(gap) =>
      bounds.max
        .getOrRaiseError(errors.withCenterStrategy)
        .map(space =>
          placeCenterMany(elements.map(_ + gap), space)
            .map(_.coordinateOfStart + (gap / F.fromInt(2)))
        )
    case MainAxisPlacementStrategy.End(gap) =>
      bounds.max
        .getOrRaiseError(errors.withEndStrategy)
        .map(space =>
          placeEndMany(elements.map(_ + gap), space)
            .map(_.coordinateOfStart + gap)
        )
    case MainAxisPlacementStrategy.SpaceBetween =>
      bounds.max
        .getOrRaiseError(errors.withSpaceBetweenStrategy)
        .map(space =>
          placeSpaceBetween(elements, space)
            .map(_.coordinateOfStart)
        )
    case MainAxisPlacementStrategy.SpaceAround =>
      bounds.max
        .getOrRaiseError(errors.withSpaceAroundStrategy)
        .map(space =>
          placeSpaceAround(elements, space)
            .map(_.coordinateOfStart)
        )
end mainAxisStrategyPlacement

def additionalAxisStrategyPlacement[
  Place[_] : ApplicativeErrorT[PlacementError],
  PlacementError,
  MeasurementUnit : Fractional
](
  strategy: AdditionalAxisPlacementStrategy, 
  element: MeasurementUnit, 
  space : AxisBounds[MeasurementUnit],
  errors : ElementPlacementInInfiniteContainerAttemptError[PlacementError]
): Place[MeasurementUnit] =
  strategy match
    case AdditionalAxisPlacementStrategy.Begin => 
      placeBegin.pure[Place]
    case AdditionalAxisPlacementStrategy.Center =>
      space
        .max
        .getOrRaiseError(errors.withCenterStrategy)
        .map(space =>
          placeCenter(element, space)
        )
    case AdditionalAxisPlacementStrategy.End =>
      space
        .max
        .getOrRaiseError(errors.withEndStrategy)
        .map(space =>
          placeEnd(element, space)
        )
  end match
end additionalAxisStrategyPlacement

//RENAMEME
def unpack[MeasurementUnit, T](lst: List[Placed[MeasurementUnit, T]]): List[(T, LayoutPlacementMeta[MeasurementUnit])] =
  lst.map(placedElementAsLayoutMetadata)
end unpack

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, LayoutPlacementMeta[MeasurementUnit]) =
  (placed.value, new LayoutPlacementMeta(placed))
end placedElementAsLayoutMetadata

def rowColumnPlace[
  Place[_] : Applicative,
  MeasurementUnit, 
  T
](
  elements           : List[Sized[MeasurementUnit, T]],
  bounds             : AxisDependentBounds[MeasurementUnit],
  mainAxisPlace      : (List[MeasurementUnit], AxisBounds[MeasurementUnit]) => Place[List[MeasurementUnit]],
  additionalAxisPlace: (MeasurementUnit, AxisBounds[MeasurementUnit]) => Place[MeasurementUnit],
  zLevel : MeasurementUnit,
): Place[List[Placed[MeasurementUnit, T]]] =
  Applicative[Place].map2(
    mainAxisPlace(elements.map(_.lengthAlong(bounds.axis)), bounds.mainAxis),
    elements.traverse(el => additionalAxisPlace(el.lengthAlongAnother(bounds.axis), bounds.additionalAxis))
  )(
    (mainAxisCoordinates, crossAxisCoordinates) =>
      Monad[List].map(elements.zip(combineCoordinates(bounds.axis, mainAxisCoordinates, crossAxisCoordinates, zLevel)))(new Placed(_, _))
  )
end rowColumnPlace

def combineCoordinates[MeasurementUnit](axis : Axis, mainAxis : List[MeasurementUnit], additionalAxis : List[MeasurementUnit], zLevel : MeasurementUnit) : List[Point3d[MeasurementUnit]] =
  if axis == Axis.Vertical then
    additionalAxis.zip(mainAxis).map((x, y) => Point3d(x, y, zLevel))
  else
    mainAxis.zip(additionalAxis).map((x, y) => Point3d(x, y, zLevel))
  end if
end combineCoordinates