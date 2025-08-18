package me.katze.gui4s
package layout.rowcolumn

import catnip.Zip
import catnip.Zip.zip
import cats.*
import cats.syntax.all.*
import me.katze.gui4s.geometry.{Axis, InfinityOr, Point2d}
import me.katze.gui4s.layout.linear.* 

type ManyElementsPlacementStrategy[Place[_], Bounds, Container[_], Point] =
    (Container[Point], Bounds) => Place[(coordinateOfEnd : Point, coordinatesOfStarts : Container[Point])]

object ManyElementsPlacementStrategy:
    def Begin[
        Place[_] : Applicative,
        Bounds,
        Container[_] : Traverse,
        MeasurementUnit : Numeric
    ](gap : MeasurementUnit) : ManyElementsPlacementStrategy[Place, Bounds, Container, MeasurementUnit] =
        (children, _) =>
            val placedChildren = placeBeginManyWithGap(children, gap)
            val size = placedChildren.map(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(Numeric[MeasurementUnit].zero)
            (size, placedChildren.map(_.coordinateOfTheBeginning)).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Fractional as MUF,
    ](
        gap : MeasurementUnit,
    ) : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            (maxSpace, placeCenterManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)).pure[Place]
    end Center

    def End[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Numeric
    ](
        gap : MeasurementUnit,
    ) : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            (maxSpace, placeEndManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)).pure[Place]
    end End

    def SpaceAround[
        Place[_] : Applicative,
        Container[_] : {Traverse, Applicative as A, SemigroupK},
        MeasurementUnit : Fractional,
    ] : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            (maxSpace, A.map(placeSpaceAround(children, maxSpace))(_.coordinateOfTheBeginning)).pure[Place]
    end SpaceAround

    def SpaceBetween[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Fractional,
    ] : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            (maxSpace, placeSpaceBetween(children, maxSpace).map(_.coordinateOfTheBeginning)).pure[Place]
    end SpaceBetween

    def OneByOne[
        Place[_] : Applicative,
        BoundsUnit,
        Container[_] : Traverse,
        MeasurementUnit : Numeric as measurementUnitsAreNumbers,
    ](
        oneElementPlacementStrategy : OneElementPlacementStrategy[Place, BoundsUnit, MeasurementUnit],
    ) : ManyElementsPlacementStrategy[Place, BoundsUnit, Container, MeasurementUnit] =
        (elements, bounds) =>
            elements.traverse(oneElementPlacementStrategy(_, bounds)).map(
                placedElements =>
                    (
                        placedElements.map(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(measurementUnitsAreNumbers.zero),
                        placedElements.map(_.coordinateOfTheBeginning),
                    )
            )
    end OneByOne

    def Zip[
        Place[_] : Applicative,
        BoundsUnit,
        Container[_] : {Traverse, Zip},
        MeasurementUnit,
    ](
        axis : Axis,
        mainAxis : ManyElementsPlacementStrategy[Place, BoundsUnit, Container, MeasurementUnit],
        crossAxis : ManyElementsPlacementStrategy[Place, BoundsUnit, Container, MeasurementUnit],
    ) : ManyElementsPlacementStrategy[Place, Point2d[BoundsUnit], Container, Point2d[MeasurementUnit]] =
        (elements, bounds) =>
            Applicative[Place].map2(
                mainAxis(elements.map(_.along(axis)), bounds.along(axis)),
                crossAxis(elements.map(_.along(axis.another)), bounds.along(axis.another)),
            ) {
                case ((mainAxisCoordinateOfEnd, mainAxisElementsCoordinates), (crossAxisCoordinateOfEnd, crossAxisElementsCoordinates)) =>
                    (
                        new Point2d(
                            axis,
                            mainAxisCoordinateOfEnd,
                            crossAxisCoordinateOfEnd
                        ),
                        mainAxisElementsCoordinates.zip(crossAxisElementsCoordinates).map(new Point2d(axis, _, _))
                    )
            }
    end Zip

    def MaybeInInfiniteSpace[
        Place[_],
        Container[_],
        MeasurementUnit,
    ](
        original: ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit],
        ifInfinity: Place[(coordinateOfEnd : MeasurementUnit, coordinatesOfStarts : Container[MeasurementUnit])],
    ): ManyElementsPlacementStrategy[Place, InfinityOr[MeasurementUnit], Container, MeasurementUnit] = {
        case (itemLength, InfinityOr(Some(space))) => original(itemLength, space)
        case _ => ifInfinity
    }

    def ErrorIfInfinity[
        Place[_],
        MeasurementUnit,
        Container[_],
        Error,
    ](
        using M: MonadError[Place, Error]
    )(
        original: ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit],
        error: Error
    ): ManyElementsPlacementStrategy[Place, InfinityOr[MeasurementUnit], Container, MeasurementUnit] =
        MaybeInInfiniteSpace(original, M.raiseError(error))
    end ErrorIfInfinity
end ManyElementsPlacementStrategy
