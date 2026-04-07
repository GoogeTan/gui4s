package gui4s.core.layout

import catnip.Set.update
import cats.Applicative
import cats.Foldable
import cats.Monad
import cats.Traverse
import cats.data.StateT
import cats.syntax.all.*
import gui4s.core.geometry.Axis
import gui4s.core.geometry.Rect

type MeasurementStrategy[Place[_], Collection[_], FreeItem, PlacedItem]
= Collection[FreeItem] => Place[Collection[PlacedItem]]

object MeasurementStrategy:
  def linearMeasurementStrategy[
    Measure[_] : Monad,
    Collection[_] : Traverse,
    Bounds,
    Item,
  ](
    getBounds : Measure[Bounds],
    withBounds : (Bounds, Measure[Item]) => Measure[Item],
    updateBoundsAccordingToItem : (Bounds, Item) => Bounds,
  ) : MeasurementStrategy[Measure, Collection, Measure[Item], Item] =
    items =>
      getBounds.flatMap(
        measureItems(
          items,
          _,
          updateBoundsAccordingToItem,
          withBounds
        )
      )
  end linearMeasurementStrategy

  def independentMeasurementStrategy[
    Measure[_] : Applicative,
    Collection[_] : Traverse,
    Item,
  ] : MeasurementStrategy[Measure, Collection, Measure[Item], Item] =
    _.traverse(identity)
  end independentMeasurementStrategy

  def layeredPlace[Place[_] : Monad, Item](
                                            backgroundCount : Int,
                                            measureWithItemBounds : (Item, Place[Item]) => Place[Item]
                                          ) : MeasurementStrategy[Place, List, Place[Item], Item] =
    items =>
      val masterElement = items(backgroundCount)
      for
        masterPlaced <- masterElement
        backgroundMeasured <- items.take(backgroundCount)
          .traverse(measureWithItemBounds(masterPlaced, _))
        foregroundMeasured <- items.drop(backgroundCount + 1)
          .traverse(measureWithItemBounds(masterPlaced, _))
      yield backgroundMeasured ++ (masterPlaced :: foregroundMeasured)
  end layeredPlace

  def weightedLinearMeasurementStrategy[
    Measure[_] : Monad,
    BoundUnit,
    MeasurementUnit : Numeric,
    Item
  ](
    getBounds: Measure[Rect[BoundUnit]],
    withBounds: (Rect[BoundUnit], Measure[Measured[Rect[MeasurementUnit], Rect[BoundUnit], Item]]) => Measure[Measured[Rect[MeasurementUnit], Rect[BoundUnit], Item]],
    cut : (BoundUnit, MeasurementUnit) => BoundUnit,
    weightedSize : (BoundUnit, Float) => BoundUnit,
    mainAxis : Axis,
  ) : MeasurementStrategy[
    Measure, 
    List, 
    Weighted[Measure[Measured[Rect[MeasurementUnit], Rect[BoundUnit], Item]]], 
    Measured[Rect[MeasurementUnit], Rect[BoundUnit], Item]
  ] =
    children =>
      for
        (rigidChildren, weightedChildren) = splitChildren(children)
        sizedRigidItems <- linearMeasurementStrategy[
          Measure, 
          List, 
          Rect[BoundUnit],
          Measured[Rect[MeasurementUnit], Rect[BoundUnit], Item]
        ](
          getBounds = getBounds,
          withBounds = withBounds,
          updateBoundsAccordingToItem = (bounds, item) => bounds.mapAlong(mainAxis, cut(_, item.size.along(mainAxis))),
        )(rigidChildren)

        totalRigidSizeAlongMainAxis = sizedRigidItems.map(_.size).map(_.along(mainAxis)).sum
        bounds <- getBounds
        remainingSize = cut(bounds.along(mainAxis), totalRigidSizeAlongMainAxis)

        sizedWeightedItems <- measureWeighted[
          Measure, List, Measured[Rect[MeasurementUnit], Rect[BoundUnit], Item], BoundUnit, MeasurementUnit
        ](getBounds, withBounds, weightedSize, mainAxis, remainingSize)(weightedChildren)

        itemsPositions = children.map(_.isRigid)

        allSizedItems = zipByBoolean(sizedRigidItems, sizedWeightedItems, itemsPositions)
      yield allSizedItems
  end weightedLinearMeasurementStrategy

  /*
  def weightedLinearMeasurementStrategy2[
    Place[_] : Monad,
    FreeItem,
    PlacedItem
  ](
    rigidMeasure : MeasurementStrategy[Place, List, FreeItem, PlacedItem],
    weightedMeasure : MeasurementStrategy[Place, List, (FreeItem, Float), PlacedItem]
  ) : MeasurementStrategy[Place, List, Weighted[FreeItem], PlacedItem] =
    children =>
      for
        (rigidChildren, weightedChildren) = splitChildren(children)
        sizedRigidItems <- rigidMeasure(rigidChildren)
        sizedWeightedItems <- weightedMeasure(weightedChildren)
      yield zipByBoolean(sizedRigidItems, sizedWeightedItems, children.map(_.isRigid))
  end weightedLinearMeasurementStrategy2*/

  def measureWeighted[
    Place[_] : Monad,
    Collection[_] : {Traverse, Foldable},
    Widget,
    BoundUnit,
    MeasurementUnit: Numeric as MUN
  ](
    getBounds: Place[Rect[BoundUnit]],
    withBounds: (Rect[BoundUnit], Place[Widget]) => Place[Widget],
    weightedSize: (BoundUnit, Float) => BoundUnit,
    mainAxis: Axis,
    remainingSize: BoundUnit,
  ): MeasurementStrategy[
    Place,
    Collection,
    (Place[Widget], Float),
    Widget
  ] =
    children =>
      for
        originalBounds <- getBounds
        totalWeight = children.map(_._2).sumAll
        sizedWeightedChildren <- children.traverse((widget, weight) =>
          withBounds(originalBounds.withLengthAlong(mainAxis, weightedSize(remainingSize, weight / totalWeight)), widget)
        )
      yield sizedWeightedChildren
  end measureWeighted


  def splitChildren[T](items: List[Weighted[T]]): (List[T], List[(T, Float)]) =
    items.partitionMap:
      case Weighted.Rigid(item) => Left(item)
      case Weighted.Weight(item, weight) => Right((item, weight))
  end splitChildren

  def zipByBoolean[T](a: List[T], b: List[T], predicates: List[Boolean]): List[T] =
    assert((a.size + b.size) == predicates.size, s"Lists length must add up to ${a.size + b.size}")
    assert(a.size == predicates.count(_ == true), s"True predicates count must be equal to ${a.size}")
    (a, b, predicates) match
      case (Nil, Nil, Nil) => Nil
      case (a :: as, b, true :: ps) => a :: zipByBoolean(as, b, ps)
      case (a, b :: bs, false :: ps) => b :: zipByBoolean(a, bs, ps)
      case _ => throw new IllegalArgumentException("Lists must have the same length. Should be impossible to reach this case.")
    end match
  end zipByBoolean

  /**
   * Измеряет размеры виджетов, изменяя количество свободного пространства в соответствии с их размерами.
   *
   * @param updateBounds Обновляет ограничения на размеры виджета
   */
  def measureItems[
    Measure[_] : Monad,
    Collection[_] : Traverse,
    Item,
    Bounds,
  ](
    items: Collection[Measure[Item]],
    initialBounds : Bounds,
    updateBounds: (Bounds, Item) => Bounds,
    withBounds : (Bounds, Measure[Item]) => Measure[Item]
  ): Measure[Collection[Item]] =
    items.traverse[StateT[Measure, Bounds, *], Item](
      freeItem =>
        for
          bounds <- StateT.get[Measure, Bounds]
          item <- StateT.liftF(withBounds(bounds, freeItem))
          _ <- StateT.set(updateBounds(bounds, item))
        yield item  
    ).runA(initialBounds)
  end measureItems
end MeasurementStrategy

