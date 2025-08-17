package me.katze.gui4s.layout.rowcolumn

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.bound.*
import catnip.syntax.all.*
import me.katze.gui4s.geometry.InfinityOr
import me.katze.gui4s.geometry.*

type OneElementPlacementStrategy[Place[_], MeasurementUnit] = (itemLength : MeasurementUnit, bounds : InfinityOr[MeasurementUnit]) => Place[Rect1dOnPoint1d[MeasurementUnit]]

object OneElementPlacementStrategy:
    def Const[Place[_] : Applicative, MeasurementUnit](whereToPlace : MeasurementUnit) : OneElementPlacementStrategy[Place, MeasurementUnit] =
        (itemLength, _) => Rect1dOnPoint1d(itemLength, whereToPlace).pure[Place]
    end Const

    def Begin[Place[_] : Applicative, MeasurementUnit : Numeric] : OneElementPlacementStrategy[Place, MeasurementUnit] =
        (itemLength, _) =>
            Rect1dOnPoint1d(
                coordinateOfTheBeginning = placeBegin[MeasurementUnit],
                length = itemLength
            ).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using MonadError[Place, Error]
    ) : OneElementPlacementStrategy[Place, MeasurementUnit] =
        (itemLength, bounds) =>
            bounds
              .value 
                .map(space =>
                    Rect1dOnPoint1d(
                        length = itemLength,
                        coordinateOfTheBeginning = placeCenter(itemLength, space)
                    )
                )
                .getOrRaiseError(errorWhenInfiniteSpace)
    end Center

    def End[
        Place[_] : Applicative,
        MeasurementUnit : Numeric,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using MonadError[Place, Error]
    ) : OneElementPlacementStrategy[Place, MeasurementUnit] =
        (itemLength, bounds) =>
            bounds
              .value
                .map(
                    space =>
                        Rect1dOnPoint1d(
                            length = space,
                            coordinateOfTheBeginning = placeEnd(itemLength, space)
                        )
                )
                .getOrRaiseError(errorWhenInfiniteSpace)
    end End
end OneElementPlacementStrategy
