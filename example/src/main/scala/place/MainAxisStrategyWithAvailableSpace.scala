package me.katze.gui4s.example
package place

import me.katze.gui4s.widget.library.MainAxisPlacementStrategy

enum MainAxisStrategyWithAvailableSpace[+MeasurementUnit]:
  case Begin(gap : MeasurementUnit)
  case Center(gap : MeasurementUnit, size : MeasurementUnit)
  case End(gap : MeasurementUnit, size : MeasurementUnit)
  case SpaceBetween(size : MeasurementUnit) 
  case SpaceAround(size : MeasurementUnit)
end MainAxisStrategyWithAvailableSpace

// TODO RENAME ME Переименовать поля, чтобы они были понятными
final case class MainAxisStrategyErrors(
  spaceBetweenInInfiniteContainer : String,
  spaceAroundInInfiniteContainer : String,
  spaceCenterInInfiniteContainer : String,
  spaceEndInInfiniteContainer : String,
)

// TODO Переписать с использованием Either
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
def unsafeSizedStrategy[MeasurementUnit : Fractional](strategy: MainAxisPlacementStrategy[MeasurementUnit], bounds : Option[MeasurementUnit], errors : MainAxisStrategyErrors) : MainAxisStrategyWithAvailableSpace[MeasurementUnit] =
  strategy match
    case MainAxisPlacementStrategy.SpaceBetween =>
      MainAxisStrategyWithAvailableSpace.SpaceBetween(bounds.getOrElse(throw Exception(errors.spaceBetweenInInfiniteContainer)))
    case MainAxisPlacementStrategy.SpaceAround =>
      MainAxisStrategyWithAvailableSpace.SpaceAround(bounds.getOrElse(throw Exception(errors.spaceAroundInInfiniteContainer)))
    case MainAxisPlacementStrategy.Begin(gap) =>
      MainAxisStrategyWithAvailableSpace.Begin(gap)
    case MainAxisPlacementStrategy.Center(gap) =>
      MainAxisStrategyWithAvailableSpace.Center(gap, bounds.getOrElse(throw Exception(errors.spaceCenterInInfiniteContainer)))
    case MainAxisPlacementStrategy.End(gap) =>
      MainAxisStrategyWithAvailableSpace.End(gap, bounds.getOrElse(throw Exception(errors.spaceEndInInfiniteContainer)))
  end match
end unsafeSizedStrategy
