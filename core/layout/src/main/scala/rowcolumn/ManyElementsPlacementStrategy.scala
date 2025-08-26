package gui4s.core.layout
package rowcolumn

import catnip.Zip
import catnip.Zip.zip
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, InfinityOr, Point2d, Rect}
import gui4s.core.layout.linear.* 

final case class ElementPlacementResult[Container[_], Point](coordinateOfEnd : Point, coordinatesOfStarts : Container[Point])

type ManyElementsPlacementStrategy[Place[_], Bounds, Container[_], Point] =
    (Container[Point], Bounds) => Place[ElementPlacementResult[Container, Point]]

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
            ElementPlacementResult(size, placedChildren.map(_.coordinateOfTheBeginning)).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Fractional as MUF,
    ](
        gap : MeasurementUnit,
    ) : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, placeCenterManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)).pure[Place]
    end Center

    def End[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Numeric
    ](
        gap : MeasurementUnit,
    ) : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, placeEndManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)).pure[Place]
    end End

    def SpaceAround[
        Place[_] : Applicative,
        Container[_] : {Traverse, Applicative as A, SemigroupK},
        MeasurementUnit : Fractional,
    ] : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, A.map(placeSpaceAround(children, maxSpace))(_.coordinateOfTheBeginning)).pure[Place]
    end SpaceAround

    def SpaceBetween[
        Place[_] : Applicative,
        Container[_] : Traverse,
        MeasurementUnit : Fractional,
    ] : ManyElementsPlacementStrategy[Place, MeasurementUnit, Container, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, placeSpaceBetween(children, maxSpace).map(_.coordinateOfTheBeginning)).pure[Place]
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
                    ElementPlacementResult(
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
    ) : ManyElementsPlacementStrategy[Place, Rect[BoundsUnit], Container, Point2d[MeasurementUnit]] =
        (elements, bounds) =>
            Applicative[Place].map2(
                mainAxis(elements.map(_.along(axis)), bounds.along(axis)),
                crossAxis(elements.map(_.along(axis.another)), bounds.along(axis.another)),
            ) {
                case (ElementPlacementResult(mainAxisCoordinateOfEnd, mainAxisElementsCoordinates), ElementPlacementResult(crossAxisCoordinateOfEnd, crossAxisElementsCoordinates)) =>
                    ElementPlacementResult(
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
        ifInfinity: Place[ElementPlacementResult[Container, MeasurementUnit]],
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
