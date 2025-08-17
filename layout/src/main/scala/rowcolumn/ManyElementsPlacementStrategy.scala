package me.katze.gui4s
package layout.rowcolumn

import catnip.syntax.all.*
import cats.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.InfinityOr
import me.katze.gui4s.layout.bound.*
import me.katze.gui4s.layout.linear.* 

type ManyElementsPlacementStrategy[Place[_], Container[_], MeasurementUnit] =
    (Container[MeasurementUnit], InfinityOr[MeasurementUnit]) => Place[(coordinateOfEnd : MeasurementUnit, children : Container[MeasurementUnit])]

object ManyElementsPlacementStrategy:
    def Begin[
        Place[_] : Applicative, 
        Container[_] : Traverse,
        MeasurementUnit : Numeric
    ](gap : MeasurementUnit) : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit] =
        (children, _) =>
            val placedChildren = placeBeginManyWithGap(children, gap)
            val size = placedChildren.map(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(Numeric[MeasurementUnit].zero)
            (size, placedChildren.map(_.coordinateOfTheBeginning)).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Fractional as MUF,
        Error
    ](
        gap : MeasurementUnit,
        errorWhenInfiniteSpace : Error
    )(
        using MonadError[Place, Error]
    ) : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit] =
        (children, bounds) =>
            bounds
                .value
                .map(space => (space, placeCenterManyWithGap(children, space, gap).map(_.coordinateOfTheBeginning)))
                .getOrRaiseError(errorWhenInfiniteSpace)
    end Center

    def End[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Numeric,
        Error
    ](
        gap : MeasurementUnit,
        errorWhenInfiniteSpace : Error
    )(
        using MonadError[Place, Error]
    ) : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit] =
        (children, bounds) =>
            bounds
                .value
                .map(maxSpace => (maxSpace, placeEndManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)))
                .getOrRaiseError(errorWhenInfiniteSpace)
    end End

    def SpaceAround[
        Place[_] : Applicative,
        Container[_] : {Traverse, Applicative as A, SemigroupK},
        MeasurementUnit : Fractional,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using MonadError[Place, Error]
    ) : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit] =
        (children, bounds) =>
        bounds
            .value
            .map(maxSpace => (maxSpace, A.map(placeSpaceAround(children, maxSpace))(_.coordinateOfTheBeginning)))
            .getOrRaiseError(errorWhenInfiniteSpace)
    end SpaceAround

    def SpaceBetween[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Fractional,
        Error
    ](
        errorWhenInfiniteSpace : Error
    )(
        using MonadError[Place, Error]
    ) : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit] =
        (children, bounds) =>
        bounds
            .value
            .map(maxSpace => (maxSpace, placeSpaceBetween(children, maxSpace).map(_.coordinateOfTheBeginning)))
            .getOrRaiseError(errorWhenInfiniteSpace)
    end SpaceBetween
end ManyElementsPlacementStrategy
