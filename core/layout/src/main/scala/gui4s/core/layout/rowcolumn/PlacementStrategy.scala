package gui4s.core.layout
package rowcolumn

import catnip.Zip
import catnip.Zip.zip
import catnip.syntax.all.given
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.linear.*


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

    def PlaceIndependently[
        Place[_] : Applicative,
        Size,
        BoundsUnit,
        Collection[_] : Traverse,
        MeasurementUnit
    ](
      oneElementPlacementStrategy : OneElementPlacementStrategy[Place, Size, BoundsUnit, MeasurementUnit],
      combineSizes : Collection[Size] => Size,
    ) : PlacementStrategy[Place, Size, BoundsUnit, Collection, MeasurementUnit] =
        (elements, bounds) =>
            elements.traverse(oneElementPlacementStrategy(_, bounds)).map(
                placedElements =>
                    ElementPlacementResult(
                        combineSizes(placedElements.map(_.size)),
                        placedElements.map(_.coordinates),
                    )
            )
    end PlaceIndependently

    def PlaceListIndependently[
      Place[_] : Applicative,
      Size : Numeric,
      BoundsUnit,
      Collection[_] : Traverse,
      MeasurementUnit,
    ](
      oneElementPlacementStrategy : OneElementPlacementStrategy[Place, Size, BoundsUnit, MeasurementUnit],
    ) : PlacementStrategy[Place, Size, BoundsUnit, Collection, MeasurementUnit] =
        PlaceIndependently(
            oneElementPlacementStrategy,
            _.maximumOption(using Order.fromOrdering(using summon)).getOrElse(Numeric[Size].zero)
        )
    end PlaceListIndependently

    def PlaceStackIndependently[
      Place[_] : Applicative,
      Size: Numeric as N,
      BoundsUnit,
      Collection[_] : Traverse,
      MeasurementUnit,
    ](
      placementStrategy: OneElementPlacementStrategy[Place, Rect[Size], Rect[BoundsUnit], Point2d[MeasurementUnit]],
    ): PlacementStrategy[Place, Rect[Size], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
        PlaceIndependently(
            placementStrategy,
            _.foldLeft(Rect(N.zero, N.zero))((res, currentPoint) =>
                Rect(
                    N.max(res.width, currentPoint.width),
                    N.max(res.height, currentPoint.height)
                )
            )
        )
    end PlaceStackIndependently

    def Stack[
        Place[_] : Applicative,
        Size : Numeric as N,
        BoundsUnit,
        Collection[_] : Traverse,
        MeasurementUnit : Numeric,
    ](
      axis : Axis,
      mainAxis : OneElementPlacementStrategy[Place, Size, BoundsUnit, MeasurementUnit],
      additionalAxis : OneElementPlacementStrategy[Place, Size, BoundsUnit, MeasurementUnit],
    ) : PlacementStrategy[Place, Rect[Size], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
        PlaceStackIndependently(
            Zip(
                axis,
                mainAxis,
                additionalAxis
            )
        )
    end Stack

    def Zip[
      Place[_] : Applicative,
      OneDimensionalSize,
      BoundsUnit,
      Collection[_] : {Traverse, Zip},
      MeasurementUnit,
    ](
      horizonal : PlacementStrategy[Place, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
      vertical : PlacementStrategy[Place, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
    ) : PlacementStrategy[Place, Rect[OneDimensionalSize], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
        (elements, bounds) =>
            Applicative[Place].map2(
                horizonal(elements.map(_.width), bounds.width),
                vertical(elements.map(_.height), bounds.height),
            ) {
                case (ElementPlacementResult(xSize, xCoordinates), ElementPlacementResult(ySize, yCoordinates)) =>
                    ElementPlacementResult(
                        new Rect(xSize, ySize),
                        xCoordinates.zip(yCoordinates).map(new Point2d(_, _))
                    )
            }

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
      if axis == Axis.Horizontal then
        Zip(mainAxis, crossAxis)
      else 
        Zip(crossAxis, mainAxis)    
      end if  
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
