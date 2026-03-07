package gui4s.core.layout

import catnip.Set.update
import cats.{Applicative, Foldable, Monad, Traverse}
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, Rect}

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
    setBounds : Bounds => Measure[Unit],
    updateBoundsAccordingToItem : (Bounds, Item) => Bounds,
  ) : MeasurementStrategy[Measure, Collection, Measure[Item], Item] =
    items =>
      for
        initialBounds <- getBounds
        res <- measureItemsDirty[Measure, Collection, Item](
          item => update(getBounds, setBounds)(updateBoundsAccordingToItem(_, item)),
          items,
        )
        _ <- setBounds(initialBounds)
      yield res
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
    setBounds: Rect[BoundUnit] => Measure[Unit],
    cut : (BoundUnit, MeasurementUnit) => BoundUnit,
    weightedSize : (BoundUnit, Float) => BoundUnit,
    mainAxis : Axis,
  ) : MeasurementStrategy[Measure, List, Weighted[Measure[Measured[MeasurementUnit, BoundUnit, Item]]], Measured[MeasurementUnit, BoundUnit, Item]] =
    children =>
      for
        (rigidChildren, weightedChildren) = splitChildren(children)
        sizedRigidItems <- measureItemsOneByOne[Measure, List, Rect[BoundUnit], Measured[MeasurementUnit, BoundUnit, Item]](
          getBounds = getBounds,
          setBounds = setBounds,
          updateBoundsAccordingToItem = (bounds, item) => bounds.mapAlong(mainAxis, cut(_, item.size.along(mainAxis))),
          items = rigidChildren
        )

        totalRigidSizeAlongMainAxis = sizedRigidItems.map(_.size).map(_.along(mainAxis)).sum
        bounds <- getBounds
        remainingSize = cut(bounds.along(mainAxis), totalRigidSizeAlongMainAxis)

        sizedWeightedItems <- measureWeighted[
          Measure, List, Measured[MeasurementUnit, BoundUnit, Item], BoundUnit, MeasurementUnit
        ](getBounds, setBounds, weightedSize, mainAxis, remainingSize)(weightedChildren)

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
    setBounds: Rect[BoundUnit] => Place[Unit],
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
          setBounds(originalBounds.withLengthAlong(mainAxis, weightedSize(remainingSize, weight / totalWeight)))
            *> widget
        )
        _ <- setBounds(originalBounds)
      yield sizedWeightedChildren
  end measureWeighted
end MeasurementStrategy

