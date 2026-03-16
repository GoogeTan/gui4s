package gui4s.core.layout

import scala.math.Numeric.Implicits._

import cats._
import cats.syntax.all._

import gui4s.core.geometry._
import gui4s.core.layout.linear._

type OneElementPlacementStrategy[Place[_], Item, Size, Bounds, Point] = PlacementStrategy[Place, Item, Size, Bounds, Id, Point]

object OneElementPlacementStrategy:
    def Const[
      Place[_] : Applicative,
      BoundsUnit,
      MeasurementUnit : Numeric
    ](whereToPlace : MeasurementUnit) : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundsUnit, MeasurementUnit] =
        (itemLength, _) =>
            Sized(whereToPlace, whereToPlace + itemLength).pure[Place]
    end Const

    def Begin[
      Place[_] : Applicative,
      BoundsUnit,
      MeasurementUnit : Numeric as N
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundsUnit, MeasurementUnit] =
      PlacementStrategy.Begin[Place, BoundsUnit, Id, MeasurementUnit](N.zero)
    end Begin

    def Center[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit] =
        (itemLength, space) =>
            val coordinatesOfStart = placeCenter(itemLength, space)
            Sized(
                coordinatesOfStart,
                coordinatesOfStart + itemLength,
            ).pure[Place]
    end Center

    def End[
        Place[_] : Applicative,
        MeasurementUnit : Numeric,
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit] =
        (itemLength, space) =>
            val coordinatesOfStart = placeEnd(itemLength, space)
            Sized(
                coordinatesOfStart,
                coordinatesOfStart + itemLength
            ).pure[Place]
    end End
    
    def MaybeInInfiniteSpace[
        Place[_],
        MeasurementUnit,
    ](
        original : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit],
        ifInfinity : Place[Sized[MeasurementUnit, MeasurementUnit]],
    ) : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, InfinityOr[MeasurementUnit], MeasurementUnit] = {
        case (itemLength, InfinityOr(Some(space))) => original(itemLength, space)
        case _ => ifInfinity  
    }
    
    def ErrorIfInfinity[
        Place[_],
        MeasurementUnit,
        Error,
    ](
        using M : MonadError[Place, Error]
    )(
        original : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit],
        error : Error
    ) : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, InfinityOr[MeasurementUnit], MeasurementUnit] =
        MaybeInInfiniteSpace(original, M.raiseError(error))
    end ErrorIfInfinity
end OneElementPlacementStrategy
