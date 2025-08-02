package me.katze.gui4s
package layout.rowcolumn

import catnip.syntax.all.*
import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.bound.*
import me.katze.gui4s.layout.linear.* 

type MainAxisPlacement[Place[_], MeasurementUnit] = 
    (List[MeasurementUnit], AxisBounds[MeasurementUnit]) => Place[(coordinateOfEnd : MeasurementUnit, children : List[MeasurementUnit])]

object MainAxisPlacement:
    def Begin[Place[_] : Applicative, MeasurementUnit : Numeric](gap : MeasurementUnit) : MainAxisPlacement[Place, MeasurementUnit] =
        (children, _) =>
            val placedChildren = placeBeginManyWithGap(children, gap)
            val size = placedChildren.map(_.coordinateOfEnd).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
            (size, placedChildren.map(_.coordinateOfStart)).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        MeasurementUnit : Fractional as MUF,
        Error
    ](
        gap : MeasurementUnit,
        errorWhenInfiniteSpace : Error
    )(
        using ME:  MonadError[Place, Error]
    ) : MainAxisPlacement[Place, MeasurementUnit] =
        (children, bounds) =>
            bounds
              .max
              .map(space => (space, placeCenterManyWithGap(children, space, gap).map(_.coordinateOfStart)))
              .getOrRaiseError(errorWhenInfiniteSpace)
    end Center

    def End[
        Place[_] : Applicative,
        MeasurementUnit : Numeric,
        Error
    ](
        gap : MeasurementUnit,
        errorWhenInfiniteSpace : Error
    )(
        using ME:  MonadError[Place, Error]
    ) : MainAxisPlacement[Place, MeasurementUnit] =
        (children, bounds) =>
        bounds
            .max
            .map(maxSpace => (maxSpace, placeEndManyWithGap(children, maxSpace, gap).map(_.coordinateOfStart)))
            .getOrRaiseError(errorWhenInfiniteSpace)
    end End


    def SpaceAround[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using ME:  MonadError[Place, Error]
    ) : MainAxisPlacement[Place, MeasurementUnit] =
        (children, bounds) =>
        bounds
            .max
            .map(maxSpace => (maxSpace, placeSpaceAround(children, maxSpace).map(_.coordinateOfStart)))
            .getOrRaiseError(errorWhenInfiniteSpace)
    end SpaceAround

    def SpaceBetween[
        Place[_] : Applicative,
        MeasurementUnit : Fractional,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using ME:  MonadError[Place, Error]
    ) : MainAxisPlacement[Place, MeasurementUnit] =
        (children, bounds) =>
        bounds
            .max
            .map(maxSpace => (maxSpace, placeSpaceBetween(children, maxSpace).map(_.coordinateOfStart)))
            .getOrRaiseError(errorWhenInfiniteSpace)
    end SpaceBetween
end MainAxisPlacement
