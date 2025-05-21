package me.katze.gui4s.example
package place

import api.LayoutPlacementMeta

import cats.*
import me.katze.gui4s.layout
import me.katze.gui4s.layout.bound.{AxisBounds, AxisDependentBounds}
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.{Placed, Sized}
import me.katze.gui4s.widget.library.{AdditionalAxisPlacementStrategy, MainAxisPlacementStrategy}
import scala.math.Fractional.Implicits.*
//import scala.math.Numeric.Implicits.*

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
  (placed.value, LayoutPlacementMeta(placed.x, placed.y))
end placedElementAsLayoutMetadata

def rowColumnPlace[MeasurementUnit, T](
                                        elements           : List[Sized[MeasurementUnit, T]],
                                        bounds             : AxisDependentBounds[MeasurementUnit],
                                        mainAxisPlace      : (List[MeasurementUnit], AxisBounds[MeasurementUnit]) => List[MeasurementUnit],
                                        additionalAxisPlace: (MeasurementUnit, AxisBounds[MeasurementUnit]) => MeasurementUnit,
                                      ): List[Placed[MeasurementUnit, T]] =
  val ys = mainAxisPlace(elements.map(_.lengthAlong(bounds.axis)), bounds.mainAxis)
  val xs = elements.map(el => additionalAxisPlace(el.lengthAlongAnother(bounds.axis), bounds.additionalAxis))

  Monad[List].map(xs.zip(ys).zip(elements))((coords, value) => Placed(value.value, coords._1, coords._2, value.width, value.height))
end rowColumnPlace