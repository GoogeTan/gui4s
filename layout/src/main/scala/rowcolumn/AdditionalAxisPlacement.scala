package me.katze.gui4s.layout.rowcolumn

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.bound.*
import catnip.syntax.all.* 

type AdditionalAxisPlacement[Place[_], MeasurementUnit] = (itemLength : MeasurementUnit, bounds : AxisBounds[MeasurementUnit]) => Place[Rect1dOnPoint1d[MeasurementUnit]]

object AdditionalAxisPlacement:
    def Begin[Place[_] : Applicative, MeasurementUnit : Numeric] : AdditionalAxisPlacement[Place, MeasurementUnit] =
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
    ) : AdditionalAxisPlacement[Place, MeasurementUnit] =
        (itemLength, bounds) =>
            bounds
                .maximumLimit
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
    ) : AdditionalAxisPlacement[Place, MeasurementUnit] =
        (itemLength, bounds) =>
            bounds
                .maximumLimit
                .map(
                    space =>
                        Rect1dOnPoint1d(
                            length = space,
                            coordinateOfTheBeginning = placeEnd(itemLength, space)
                        )
                )
                .getOrRaiseError(errorWhenInfiniteSpace)
    end End
end AdditionalAxisPlacement
