package gui4s.core.layout

import catnip.Zip
import catnip.Zip.zip
import catnip.syntax.all.given
import cats._
import cats.syntax.all._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.layout.linear._

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
type PlacementStrategy[Place[_], Item, Size, Bounds, Collection[_], Point] = 
  (Collection[Item], Bounds) => Place[Sized[Size, Collection[Point]]]

extension[Place[_], Item, Size, Bounds, Collection[_], Point](value : PlacementStrategy[Place, Item, Size, Bounds, Collection, Point])
  def andThen[Point2](f : Collection[Point] => Collection[Point2])(using Functor[Place]) : PlacementStrategy[Place, Item, Size, Bounds, Collection, Point2] =
    (items, bounds) =>
      value(items, bounds).map(_.map(f))
  end andThen
end extension

object PlacementStrategy:
  def Begin[
      Place[_] : Applicative,
      Bounds,
      Collection[_] : Traverse,
      MeasurementUnit : Numeric
  ](gap : MeasurementUnit) : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, Bounds, Collection, MeasurementUnit] =
    (children, _) =>
      val placedChildren = placeBeginManyWithGap(children, gap)
      val size = placedChildren.map(_.coordinateOfTheEnd).maximumOption(using Order.fromOrdering(using summon)).getOrElse(Numeric[MeasurementUnit].zero)
      Sized(placedChildren.map(_.coordinateOfTheBeginning), size).pure[Place]
  end Begin
    
  def Center[
    Place[_] : Applicative, 
    Collection[_] : Traverse,
    MeasurementUnit : Fractional as MUF,
  ](
    gap : MeasurementUnit,
  ) : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
    (children, maxSpace) =>
      Sized(placeCenterManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning), maxSpace).pure[Place]
  end Center

  def End[
    Place[_] : Applicative,
    Collection[_] : Traverse,
    MeasurementUnit : Numeric
  ](
    gap : MeasurementUnit,
  ) : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
    (children, maxSpace) =>
      Sized(placeEndManyWithGap(children, maxSpace, gap).map(_.coordinateOfTheBeginning), maxSpace).pure[Place]
  end End

  def SpaceAround[
    Place[_] : Applicative,
    Collection[_] : {Applicative, Traverse as A, SemigroupK},
    MeasurementUnit : Fractional,
  ] : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
    (children, maxSpace) =>
      Sized(A.map(placeSpaceAround(children, maxSpace))(_.coordinateOfTheBeginning), maxSpace).pure[Place]
  end SpaceAround

  def SpaceBetween[
    Place[_] : Applicative,
    Collection[_] : Traverse,
    MeasurementUnit : Fractional,
  ] : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit] =
    (children, maxSpace) =>
      Sized(placeSpaceBetween(children, maxSpace).map(_.coordinateOfTheBeginning), maxSpace).pure[Place]
  end SpaceBetween

  def PlaceIndependently[
    Place[_] : Applicative,
    Size,
    BoundsUnit,
    Collection[_] : Traverse,
    MeasurementUnit
  ](
    oneElementPlacementStrategy : OneElementPlacementStrategy[Place, Size, Size, BoundsUnit, MeasurementUnit],
    combineSizes : Collection[Size] => Size,
  ) : PlacementStrategy[Place, Size, Size, BoundsUnit, Collection, MeasurementUnit] =
    (elements, bounds) =>
      elements.traverse(oneElementPlacementStrategy(_, bounds)).map(
        placedElements =>
          Sized(
            placedElements.map(_.value),
            combineSizes(placedElements.map(_.size)),
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
    oneElementPlacementStrategy : OneElementPlacementStrategy[Place, Size, Size, BoundsUnit, MeasurementUnit],
  ) : PlacementStrategy[Place, Size, Size, BoundsUnit, Collection, MeasurementUnit] =
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
    placementStrategy: OneElementPlacementStrategy[Place, Rect[Size], Rect[Size], Rect[BoundsUnit], Point2d[MeasurementUnit]],
  ): PlacementStrategy[Place, Rect[Size], Rect[Size], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
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
    mainAxis : OneElementPlacementStrategy[Place, Size, Size, BoundsUnit, MeasurementUnit],
    additionalAxis : OneElementPlacementStrategy[Place, Size, Size, BoundsUnit, MeasurementUnit],
  ) : PlacementStrategy[Place, Rect[Size], Rect[Size], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
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
    horizonal : PlacementStrategy[Place, OneDimensionalSize, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
    vertical : PlacementStrategy[Place, OneDimensionalSize, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
  ) : PlacementStrategy[Place, Rect[OneDimensionalSize], Rect[OneDimensionalSize], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
    (elements, bounds) => 
      Applicative[Place].map2(
        horizonal(elements.map(_.width), bounds.width),
        vertical(elements.map(_.height), bounds.height),
      ) {
        case (Sized(xCoordinates, xSize), Sized(yCoordinates, ySize)) =>
          Sized(
            xCoordinates.zip(yCoordinates).map(new Point2d(_, _)),
            new Rect(xSize, ySize),
          )
      }
  end Zip  
  
  def Zip[
    Place[_] : Applicative,
    OneDimensionalSize,
    BoundsUnit,
    Collection[_] : {Traverse, Zip},
    MeasurementUnit,
  ](
    axis : Axis,
    mainAxis : PlacementStrategy[Place, OneDimensionalSize, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
    crossAxis : PlacementStrategy[Place, OneDimensionalSize, OneDimensionalSize, BoundsUnit, Collection, MeasurementUnit],
  ) : PlacementStrategy[Place, Rect[OneDimensionalSize], Rect[OneDimensionalSize], Rect[BoundsUnit], Collection, Point2d[MeasurementUnit]] =
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
    original: PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit],
    ifInfinity: Place[Sized[MeasurementUnit, Collection[MeasurementUnit]]],
  ): PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, InfinityOr[MeasurementUnit], Collection, MeasurementUnit] = {
    case (itemLength, InfinityOr(Some(space))) => original(itemLength, space)
    case _ => ifInfinity
  }

  def ErrorIfInfinity[
    Place[_],
    MeasurementUnit,
    Collection[_],
    Error,
  ](
    original: PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, MeasurementUnit, Collection, MeasurementUnit],
    error: Error
  )(
    using M: MonadError[Place, Error]
  ): PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, InfinityOr[MeasurementUnit], Collection, MeasurementUnit] =
    MaybeInInfiniteSpace(original, M.raiseError(error))
  end ErrorIfInfinity
    
  def MasterBasedStack[
    Place[_] : Monad,
    Item,
    Size,
    Bounds,
    Point
  ](
    elementsStrategy : PlacementStrategy[Place, Item, Size, Size, List, Point],
    masterStrategy : PlacementStrategy[Place, Item, Size, Bounds, Id, Point],
    masterIndex : Int
  ) : PlacementStrategy[Place, Item, Size, Bounds, List, Point] =
    (items, bounds) =>
      for
        master = items(masterIndex)
        placedMaster <- masterStrategy(master, bounds)
        background = items.take(masterIndex)
        foreground = items.drop(masterIndex + 1)
        itemsPlaced <- elementsStrategy(background ++ foreground, placedMaster.size)
        backgroundCoordinates = itemsPlaced.value.take(background.size)
        foregroundCoordinates = itemsPlaced.value.drop(background.size)
      yield Sized(
        backgroundCoordinates ++ (placedMaster.value :: foregroundCoordinates),
        placedMaster.size,
      )
  end MasterBasedStack
end PlacementStrategy
