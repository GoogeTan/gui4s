package me.katze.gui4s.example
package place

import api.MainAxisPlacementStrategy

// TODO RENAME ME Переименовать класс и size.
enum SizedStrategy[+MeasurementUnit]:
  case Begin(gap : MeasurementUnit)
  case Center(gap : MeasurementUnit, size : MeasurementUnit)
  case End(gap : MeasurementUnit, size : MeasurementUnit)
  case SpaceBetween(size : MeasurementUnit) 
  case SpaceAround(size : MeasurementUnit)
end SizedStrategy

final case class MainAxisStrategyErrors(
  spaceBetweenInInfiniteContainer : String,
  spaceAroundInInfiniteContainer : String,
  spaceCenterInInfiniteContainer : String,
  spaceEndInInfiniteContainer : String,
)

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
def unsafeSizedStrategy[MeasurementUnit : Fractional](strategy: MainAxisPlacementStrategy[MeasurementUnit], bounds : Option[MeasurementUnit], errors : MainAxisStrategyErrors) : SizedStrategy[MeasurementUnit] =
  strategy match
    case MainAxisPlacementStrategy.SpaceBetween =>
      SizedStrategy.SpaceBetween(bounds.getOrElse(throw Exception(errors.spaceBetweenInInfiniteContainer)))
    case MainAxisPlacementStrategy.SpaceAround =>
      SizedStrategy.SpaceAround(bounds.getOrElse(throw Exception(errors.spaceAroundInInfiniteContainer)))
    case MainAxisPlacementStrategy.Begin(gap) =>
      SizedStrategy.Begin(gap)
    case MainAxisPlacementStrategy.Center(gap) =>
      SizedStrategy.Center(gap, bounds.getOrElse(throw Exception(errors.spaceCenterInInfiniteContainer)))
    case MainAxisPlacementStrategy.End(gap) =>
      SizedStrategy.End(gap, bounds.getOrElse(throw Exception(errors.spaceEndInInfiniteContainer)))
  end match
end unsafeSizedStrategy
