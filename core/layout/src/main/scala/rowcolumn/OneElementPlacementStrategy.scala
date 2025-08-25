package gui4s.core.layout
package rowcolumn

import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.linear.*

type OneElementPlacementStrategy[Place[_], BoundsUnit, MeasurementUnit] = (itemLength : MeasurementUnit, bounds : BoundsUnit) => Place[Rect1dOnPoint1d[MeasurementUnit]]

object OneElementPlacementStrategy:
    def Const[Place[_] : Applicative, BoundsUnit, MeasurementUnit](whereToPlace : MeasurementUnit) : OneElementPlacementStrategy[Place, BoundsUnit, MeasurementUnit] =
        (itemLength, _) => Rect1dOnPoint1d(itemLength, whereToPlace).pure[Place]
    end Const

    def Begin[Place[_] : Applicative, BoundsUnit, MeasurementUnit : Numeric] : OneElementPlacementStrategy[Place, BoundsUnit, MeasurementUnit] =
        (itemLength, _) =>
            Rect1dOnPoint1d(
                coordinateOfTheBeginning = placeBegin[MeasurementUnit],
                length = itemLength
            ).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit] =
        (itemLength, space) =>
            Rect1dOnPoint1d(
                length = itemLength,
                coordinateOfTheBeginning = placeCenter(itemLength, space)
            ).pure[Place]
    end Center

    def End[
        Place[_] : Applicative,
        MeasurementUnit : Numeric,
    ] : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit] =
        (itemLength, space) =>
            Rect1dOnPoint1d(
                length = space,
                coordinateOfTheBeginning = placeEnd(itemLength, space)
            ).pure[Place]
    end End
    
    def MaybeInInfiniteSpace[
        Place[_],
        MeasurementUnit,
    ](
        original : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit],
        ifInfinity : Place[Rect1dOnPoint1d[MeasurementUnit]],
    ) : OneElementPlacementStrategy[Place, InfinityOr[MeasurementUnit], MeasurementUnit] = {
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
        original : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit],
        error : Error
    ) : OneElementPlacementStrategy[Place, InfinityOr[MeasurementUnit], MeasurementUnit] =
        MaybeInInfiniteSpace(original, M.raiseError(error))
    end ErrorIfInfinity
end OneElementPlacementStrategy
