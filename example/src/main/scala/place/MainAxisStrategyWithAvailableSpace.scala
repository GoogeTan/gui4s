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

final case class ElementPlacementInInfiniteContainerAttemptError[Error](
                                                                          withSpaceBetweenStrategy : Error,
                                                                          withSpaceAroundStrategy : Error,
                                                                          withCenterStrategy : Error,
                                                                          withEndStrategy : Error,
)

def unsafeSizedStrategy[
  F[_], 
  MeasurementUnit : Fractional,
  Error
](
  using A : ApplicativeError[F, Error]
)(
  strategy: MainAxisPlacementStrategy[MeasurementUnit], 
  bounds : Option[MeasurementUnit], 
  errors : ElementPlacementInInfiniteContainerAttemptError[Error]
) : F[MainAxisStrategyWithAvailableSpace[MeasurementUnit]] =
  strategy match
    case MainAxisPlacementStrategy.SpaceBetween =>
      bounds
        .getOrRaise(errors.withSpaceBetweenStrategy)
        .map(MainAxisStrategyWithAvailableSpace.SpaceBetween(_))
    case MainAxisPlacementStrategy.SpaceAround =>
      bounds
        .getOrRaise(errors.withSpaceAroundStrategy)
        .map(
          MainAxisStrategyWithAvailableSpace.SpaceAround(_)   
        )
    case MainAxisPlacementStrategy.Begin(gap) =>
      MainAxisStrategyWithAvailableSpace.Begin(gap).pure[F]
    case MainAxisPlacementStrategy.Center(gap) =>
      bounds
        .getOrRaise(errors.withCenterStrategy)
        .map(
          MainAxisStrategyWithAvailableSpace.Center(gap, _)
        )
    case MainAxisPlacementStrategy.End(gap) =>
      bounds
        .getOrRaise(errors.withEndStrategy)
        .map(
          MainAxisStrategyWithAvailableSpace.End(gap, _)
        )
  end match
end unsafeSizedStrategy
