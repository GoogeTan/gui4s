package me.katze.gui4s.example
package place

import api.LayoutPlacementMeta

import cats.*
import me.katze.gui4s.example.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}
import me.katze.gui4s.layout
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds}
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.{Axis, Placed, Sized}

import scala.math.Fractional.Implicits.*

def mainAxisStrategyPlacement[MeasurementUnit : Fractional as F](strategy: MainAxisStrategyWithAvailableSpace[MeasurementUnit], elements: List[MeasurementUnit]): List[MeasurementUnit] =
  strategy match
    case MainAxisStrategyWithAvailableSpace.Begin(gap) =>
      placeBeginMany(elements.map(_ + gap))
        .map(_.coordinateOfStart)
    case MainAxisStrategyWithAvailableSpace.Center(gap, space) =>
      placeCenterMany(elements.map(_ + gap), space)
        .map(_.coordinateOfStart + (gap / F.fromInt(2)))
    case MainAxisStrategyWithAvailableSpace.End(gap, space) =>
      placeEndMany(elements.map(_ + gap), space)
        .map(_.coordinateOfStart + gap)
    case MainAxisStrategyWithAvailableSpace.SpaceBetween(space) =>
      placeSpaceBetween(elements, space)
        .map(_.coordinateOfStart)
    case MainAxisStrategyWithAvailableSpace.SpaceAround(space) =>
      placeSpaceAround(elements, space)
        .map(_.coordinateOfStart)
end mainAxisStrategyPlacement

def additionalAxisStrategyPlacement[MeasurementUnit : Fractional](strategy: AdditionalAxisPlacementStrategy, element: MeasurementUnit, space : => MeasurementUnit): MeasurementUnit =
  strategy match
    case AdditionalAxisPlacementStrategy.Begin => placeBegin
    case AdditionalAxisPlacementStrategy.Center => placeCenter(element, space)
    case AdditionalAxisPlacementStrategy.End => placeEnd(element, space)
  end match
end additionalAxisStrategyPlacement

//RENAMEME
def unpack[MeasurementUnit, T](lst: List[Placed[MeasurementUnit, T]]): List[(T, LayoutPlacementMeta[MeasurementUnit])] =
  lst.map(placedElementAsLayoutMetadata)
end unpack

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, LayoutPlacementMeta[MeasurementUnit]) =
  (placed.value, new LayoutPlacementMeta(placed))
end placedElementAsLayoutMetadata

def rowColumnPlace[MeasurementUnit, T](
                                        elements           : List[Sized[MeasurementUnit, T]],
                                        bounds             : AxisDependentBounds[MeasurementUnit],
                                        mainAxisPlace      : (List[MeasurementUnit], AxisBounds[MeasurementUnit]) => List[MeasurementUnit],
                                        additionalAxisPlace: (MeasurementUnit, AxisBounds[MeasurementUnit]) => MeasurementUnit,
                                        zLevel : MeasurementUnit,
                                      ): List[Placed[MeasurementUnit, T]] =
  val mainAxisCoordinates = mainAxisPlace(elements.map(_.lengthAlong(bounds.axis)), bounds.mainAxis)
  val crossAxisCoordinates = elements.map(el => additionalAxisPlace(el.lengthAlongAnother(bounds.axis), bounds.additionalAxis))

  val compoundCoordinates =
    mainAxisCoordinates.zip(crossAxisCoordinates).map(
      (mainAxisCoordinate, additionalAxisCoordinate) =>
        if bounds.axis == Axis.Vertical then
          (x = additionalAxisCoordinate, y = mainAxisCoordinate, z = zLevel)
        else
          (x = mainAxisCoordinate, y = additionalAxisCoordinate, z = zLevel)
    )
    
  Monad[List].map(
    compoundCoordinates.zip(elements)
  )((coords, value) => Placed(value.value, coords.x, coords.y, coords.z, value.width, value.height))
end rowColumnPlace