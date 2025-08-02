package me.katze.gui4s.layout.rowcolumn

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.linear.*
import me.katze.gui4s.layout.bound.*
import catnip.syntax.all.* 

type AdditionalAxisPlacement[Place[_], MeasurementUnit] = (MeasurementUnit, AxisBounds[MeasurementUnit]) => Place[CoveredSpace[MeasurementUnit]]

object AdditionalAxisPlacement:
    def Begin[Place[_] : Applicative, MeasurementUnit : Numeric] : AdditionalAxisPlacement[Place, MeasurementUnit] =
        (widget, _) => CoveredSpace(placeBegin[MeasurementUnit], widget).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using ME:  MonadError[Place, Error]
    ) : AdditionalAxisPlacement[Place, MeasurementUnit] =
        (widget, bounds) =>
            bounds
                .max
                .map(space => CoveredSpace(placeCenter(widget, space), space))
                .getOrRaiseError(errorWhenInfiniteSpace)
    end Center

    def End[
        Place[_] : Applicative,
        MeasurementUnit : Numeric,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using ME:  MonadError[Place, Error]
    ) : AdditionalAxisPlacement[Place, MeasurementUnit] =
        (widget, bounds) =>
            bounds
                .max
                .map(space => CoveredSpace(placeEnd(widget, space), space))
                .getOrRaiseError(errorWhenInfiniteSpace)
    end End
end AdditionalAxisPlacement
