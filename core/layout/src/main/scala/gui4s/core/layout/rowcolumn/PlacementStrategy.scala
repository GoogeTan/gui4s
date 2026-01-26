package gui4s.core.layout
package rowcolumn

import catnip.Zip
import catnip.Zip.zip
import cats._
import cats.syntax.all._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.linear._ 

final case class ElementPlacementResult[Collection[_], Size, Point](size : Size, coordinates : Collection[Point])

/**
 * Функция, описывающая расстановку элементов в контейнере. Принимает множество
 * размеров виджетов и возвращает размер всего контейнера и координаты расположения виджетов.
 *
 * Размер не вычислим из положегний и размеров виджетов, так как может быть задача поставить виджеты в центр.
 * Тогда контейнер занимает пустое место, которое не занято ни одним из виджетов.
 *
 * @tparam Place Эффект размещения виджета на экран. Может использоваться для создания ошибок(например,
 * при попытке установить виджет посередине бесконечного скролла)
 */
type PlacementStrategy[Place[_], Size, Bounds, Collection[_], Point] =
    (Collection[Size], Bounds) => Place[ElementPlacementResult[Collection, Size, Point]]

object PlacementStrategy:
    def Begin[
        Place[_] : Applicative,
        Bounds,
        Collection[_] : Traverse,
        MeasurementUnit : Numeric
    ](gap : MeasurementUnit) : PlacementStrategy[Place, MeasurementUnit, Bounds, Collection, MeasurementUnit] =
        (children, _) =>
            val placedChildren = placeBeginManyWithGap(children, gap)
            val size = placedChildren.map(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(Numeric[MeasurementUnit].zero)
            ElementPlacementResult(size, placedChildren.map(_.coordinateOfTheBeginning)).pure[Place]
    end Begin

    def Center[
        Place[_] : Applicative,
        Collection[_] : Traverse,
        MeasurementUnit : Fractional as MUF,
    ](
        gap : MeasurementUnit,
    ) : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, placeCenterManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)).pure[Place]
    end Center

    def End[
        Place[_] : Applicative,
        Collection[_] : Traverse,
        MeasurementUnit : Numeric
    ](
        gap : MeasurementUnit,
    ) : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, placeEndManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning)).pure[Place]
    end End

    def SpaceAround[
        Place[_] : Applicative,
        Collection[_] : {Applicative, Traverse as A, SemigroupK},
        MeasurementUnit : Fractional,
    ] : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, A.map(placeSpaceAround(children, maxSpace))(_.coordinateOfTheBeginning)).pure[Place]
    end SpaceAround

    def SpaceBetween[
        Place[_] : Applicative,
        Collection[_] : Traverse,
        MeasurementUnit : Fractional,
    ] : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
        (children, maxSpace) =>
            ElementPlacementResult(maxSpace, placeSpaceBetween(children, maxSpace).map(_.coordinateOfTheBeginning)).pure[Place]
    end SpaceBetween

    //TODO добавить полноценный моноид для размера. Так как надо отдельно собирать максиальные размеры по ширине и по высоте
    def PlaceIndependently[
        Place[_] : Applicative,
        Size : Ordering,
        BoundsUnit,
        Collection[_] : Traverse,
        MeasurementUnit
    ](
        oneElementPlacementStrategy : OneElementPlacementStrategy[Place, Size, BoundsUnit, MeasurementUnit],
        zeroSize : Size
    ) : PlacementStrategy[Place, Size, BoundsUnit, Collection, MeasurementUnit] =
        (elements, bounds) =>
            elements.traverse(oneElementPlacementStrategy(_, bounds)).map(
                placedElements =>
                    ElementPlacementResult(
                        placedElements.map(_.size).maximumOption(using Order.fromOrdering(using summon)).getOrElse(zeroSize),
                        placedElements.map(_.coordinates),
                    )
            )
    end PlaceIndependently

    def Zip[
        Place[_] : Applicative,
        OneDimensionalSize,
        BoundsUnit,
        Collection[_] : {Traverse, Zip},
        MeasurementUnit,
    ](
        axis : Axis,
        mainAxis : PlacementStrategy[Place, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
        crossAxis : PlacementStrategy[Place, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
    ) : PlacementStrategy[Place, Rect[OneDimensionalSize], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
        (elements, bounds) =>
            Applicative[Place].map2(
                mainAxis(elements.map(_.along(axis)), bounds.along(axis)),
                crossAxis(elements.map(_.along(axis.another)), bounds.along(axis.another)),
            ) {
                case (ElementPlacementResult(mainAxisCoordinateOfEnd, mainAxisElementsCoordinates), ElementPlacementResult(crossAxisCoordinateOfEnd, crossAxisElementsCoordinates)) =>
                    ElementPlacementResult(
                        new Rect(
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
        Collection[_],
        Size,
        MeasurementUnit,
    ](
        original: PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit],
        ifInfinity: Place[ElementPlacementResult[Collection, MeasurementUnit, MeasurementUnit]],
    ): PlacementStrategy[Place, MeasurementUnit, InfinityOr[MeasurementUnit], Collection, MeasurementUnit] = {
        case (itemLength, InfinityOr(Some(space))) => original(itemLength, space)
        case _ => ifInfinity
    }

    def ErrorIfInfinity[
        Place[_],
        MeasurementUnit,
        Collection[_],
        Error,
    ](
        using M: MonadError[Place, Error]
    )(
        original: PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit],
        error: Error
    ): PlacementStrategy[Place, MeasurementUnit, InfinityOr[MeasurementUnit], Collection, MeasurementUnit] =
        MaybeInInfiniteSpace(original, M.raiseError(error))
    end ErrorIfInfinity
end PlacementStrategy
