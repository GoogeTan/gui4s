package gui4s.core.layout.rowcolumn

import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, Point2d, Rect}
import gui4s.core.layout.{Placed, Sized, Weighted}

/**
 * Расставляет дочерние виджеты линейного взвешенного контейнера.
 *
 * @param getBounds Возвращает ограничения на размеры виджета
 * @param setBounds Устанавливает ограничения на размеры виджета
 * @param cut Позволяет уменьшить ограничение по оси на заданную длину
 * @param mainAxis главная ось, по которой расставляются виджеты
 * @param children дочерние виджеты
 * @param elementsPlacement Правило установки дочерних виджетов
 * @tparam BoundUnit Тип ограничений на размеры виджета по одной из осей. Обычно это или MeasurementUnit, или [[gui4s.core.geometry.InfinityOr]][MeasurementUnit]
 * @tparam MeasurementUnit Единицы измерения размеров экрана. Это может быть пиксели, или DP, или другие единицы измерения.
 */
def weightedRowColumnLayoutPlacement[
  Place[_] : Monad,
  Widget,
  BoundUnit,
  MeasurementUnit : Numeric as MUN,
](
  getBounds: Place[Rect[BoundUnit]],
  setBounds: Rect[BoundUnit] => Place[Unit],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  weightedSize : (BoundUnit, Float) => BoundUnit,
  mainAxis : Axis,
  children : List[Weighted[Place[Sized[MeasurementUnit, Widget]]]],
  elementsPlacement : PlacementStrategy[Place, Rect[MeasurementUnit], Rect[BoundUnit], List, Point2d[MeasurementUnit]],
) : Place[Sized[MeasurementUnit, List[Placed[MeasurementUnit, Widget]]]] =
  for
    (rigidChildren, weightedChildren) = splitChildren(children)


    sizedRigidItems <- measureItemsOneByOne[Place, List, Rect[BoundUnit], Sized[MeasurementUnit, Widget]](
      getBounds = getBounds,
      setBounds = setBounds,
      updateBoundsAccordingToItem = (bounds, item) => bounds.mapAlong(mainAxis, cut(_, item.lengthAlong(mainAxis))),
      items = rigidChildren
    )

    totalRigidSizeAlongMainAxis = sizedRigidItems.map(_.size).map(_.along(mainAxis)).sum
    bounds <- getBounds
    remainingSize = cut(bounds.along(mainAxis), totalRigidSizeAlongMainAxis)

    sizedWeightedItems <- placeWeightedChildren(getBounds, setBounds, weightedSize, weightedChildren, mainAxis, remainingSize)

    itemsPositions = children.map(_.isRigid)

    allSizedItems = zipByBoolean(sizedRigidItems, sizedWeightedItems, itemsPositions)

    placedItems <- elementsPlacement(allSizedItems.map(_.size), bounds)
  yield Sized(
    allSizedItems.zip(placedItems.coordinates).map(new Placed(_, _, MUN.zero)),
    placedItems.size
  )
end weightedRowColumnLayoutPlacement

def placeWeightedChildren[Place[_] : Monad, Widget, BoundUnit, MeasurementUnit : Numeric as MUN](
                                                                                                   getBounds : Place[Rect[BoundUnit]],
                                                                                                   setBounds : Rect[BoundUnit] => Place[Unit],
                                                                                                   weightedSize : (BoundUnit, Float) => BoundUnit,
                                                                                                   children : List[(Place[Sized[MeasurementUnit, Widget]], Float)],
                                                                                                   mainAxis : Axis,
                                                                                                   remainingSize : BoundUnit,
                                                                                                 ) : Place[List[Sized[MeasurementUnit, Widget]]] =
  for
    originalBounds <- getBounds
    totalWeight = children.map(_._2).sum
    sizedWeightedChildren <- children.traverse((widget, weight) =>
      setBounds(originalBounds.withLengthAlong(mainAxis, weightedSize(remainingSize, weight / totalWeight)))
        *> widget
    )
    _ <- setBounds(originalBounds)
  yield sizedWeightedChildren
end placeWeightedChildren

def splitChildren[T](items : List[Weighted[T]]) : (List[T], List[(T, Float)]) =
  items.partitionMap:
    case Weighted.Rigid(item) => Left(item)
    case Weighted.Weight(item, weight) => Right((item, weight))
end splitChildren

def zipByBoolean[T](a : List[T], b : List[T], predicates : List[Boolean]) : List[T] =
  assert((a.size + b.size) == predicates.size, s"Lists length must add up to ${a.size + b.size}")
  assert(a.size == predicates.count(_ == true), s"True predicates count must be equal to ${a.size}")
  (a, b, predicates) match
    case (Nil, Nil, Nil) => Nil
    case (a :: as, b, true :: ps) => a :: zipByBoolean(as, b, ps)
    case (a, b :: bs, false :: ps) => b :: zipByBoolean(a, bs, ps)
    case _ => throw new IllegalArgumentException("Lists must have the same length. Should be impossible to reach this case.")
  end match
end zipByBoolean