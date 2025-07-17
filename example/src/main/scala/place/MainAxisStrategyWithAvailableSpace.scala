package me.katze.gui4s.example
package place

import cats.ApplicativeError
import me.katze.gui4s.widget.library.MainAxisPlacementStrategy
import cats.syntax.all.*
import catnip.syntax.applicative.getOrRaise

enum MainAxisStrategyWithAvailableSpace[+MeasurementUnit]:
  case Begin(gap : MeasurementUnit)
  case Center(gap : MeasurementUnit, size : MeasurementUnit)
  case End(gap : MeasurementUnit, size : MeasurementUnit)
  case SpaceBetween(size : MeasurementUnit) 
  case SpaceAround(size : MeasurementUnit)
end MainAxisStrategyWithAvailableSpace

// TODO RENAME ME Переименовать поля, чтобы они были понятными
final case class MainAxisStrategyErrors[Error](
                                                attemptedToPlaceElementsWithStrategySpaceBetweenInInfiniteContainer : Error,
                                                attemptedToPlaceElementsWithStrategySpaceAroundInInfiniteContainer : Error,
                                                attemptedToPlaceElementsWithStrategyCenterInInfiniteContainer : Error,
                                                attemptedToPlaceElementsWithStrategyEndInInfiniteContainer : Error,
)

// TODO Переписать с использованием Either
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
def unsafeSizedStrategy[
  F[_], 
  MeasurementUnit : Fractional,
  Error
](
  using A : ApplicativeError[F, Error]
)(
  strategy: MainAxisPlacementStrategy[MeasurementUnit], 
  bounds : Option[MeasurementUnit], 
  errors : MainAxisStrategyErrors[Error]
) : F[MainAxisStrategyWithAvailableSpace[MeasurementUnit]] =
  strategy match
    case MainAxisPlacementStrategy.SpaceBetween =>
      bounds
        .getOrRaise(errors.attemptedToPlaceElementsWithStrategySpaceBetweenInInfiniteContainer)
        .map(MainAxisStrategyWithAvailableSpace.SpaceBetween(_))
    case MainAxisPlacementStrategy.SpaceAround =>
      bounds
        .getOrRaise(errors.attemptedToPlaceElementsWithStrategySpaceAroundInInfiniteContainer)
        .map(
          MainAxisStrategyWithAvailableSpace.SpaceAround(_)   
        )
    case MainAxisPlacementStrategy.Begin(gap) =>
      MainAxisStrategyWithAvailableSpace.Begin(gap).pure[F]
    case MainAxisPlacementStrategy.Center(gap) =>
      bounds
        .getOrRaise(errors.attemptedToPlaceElementsWithStrategyCenterInInfiniteContainer)
        .map(
          MainAxisStrategyWithAvailableSpace.Center(gap, _)
        )
    case MainAxisPlacementStrategy.End(gap) =>
      bounds
        .getOrRaise(errors.attemptedToPlaceElementsWithStrategyEndInInfiniteContainer)
        .map(
          MainAxisStrategyWithAvailableSpace.End(gap, _)
        )
  end match
end unsafeSizedStrategy
