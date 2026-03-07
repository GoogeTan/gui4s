package gui4s.core.layout

import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.linear.*

import scala.math.Numeric.Implicits.*

type OneElementPlacementStrategy[Place[_], Item, Size, Bounds, Point] = PlacementStrategy[Place, Item, Size, Bounds, Id, Point]

object OneElementPlacementStrategy:
    def Const[
      Place[_] : Applicative,
      BoundsUnit,
      MeasurementUnit : Numeric
    ](whereToPlace : MeasurementUnit) : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundsUnit, MeasurementUnit] =
        (itemLength, _) =>
            ElementPlacementResult[Id, MeasurementUnit, MeasurementUnit](whereToPlace + itemLength, whereToPlace).pure[Place]
    end Const

    def Begin[
      Place[_] : Applicative,
      BoundsUnit,
      MeasurementUnit : Numeric as N
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundsUnit, MeasurementUnit] =
      PlacementStrategy.Begin[Place, BoundsUnit, Id, MeasurementUnit](N.zero)
    end Begin

    //TODO оно не работает вообще, исправить
    def Center[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit] =
        (itemLength, space) =>
            val coordinatesOfStart = placeCenter(itemLength, space)
            ElementPlacementResult[Id, MeasurementUnit, MeasurementUnit](
                coordinates = coordinatesOfStart,
                size = coordinatesOfStart + itemLength
            ).pure[Place]
    end Center

    def End[
        Place[_] : Applicative,
        MeasurementUnit : Numeric,
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit] =
        (itemLength, space) =>
            val coordinatesOfStart = placeEnd(itemLength, space)
            ElementPlacementResult[Id, MeasurementUnit, MeasurementUnit](
                coordinates = coordinatesOfStart,
                size = coordinatesOfStart + itemLength
            ).pure[Place]
    end End
    
    def MaybeInInfiniteSpace[
        Place[_],
        MeasurementUnit,
    ](
        original : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, MeasurementUnit],
        ifInfinity : Place[ElementPlacementResult[Id, MeasurementUnit, MeasurementUnit]],
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
