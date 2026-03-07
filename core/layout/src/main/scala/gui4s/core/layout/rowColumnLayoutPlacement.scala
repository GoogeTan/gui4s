package gui4s.core.layout

import catnip.Set.*
import catnip.Zip
import catnip.Zip.zip
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, Point2d, Rect}


/**
 * Расставляет дочерние виджеты линейного контейнера.
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
def rowColumnLayoutPlacement[
  Place[_] : Monad,
  Collection[_] : {Traverse, Zip},
  Widget,
  BoundUnit,
  MeasurementUnit : Numeric as MUN,
](
  getBounds: Place[Rect[BoundUnit]],
  setBounds: Rect[BoundUnit] => Place[Unit],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  mainAxis : Axis,
  children : Collection[Place[Widget]],
  elementsPlacement : PlacementStrategy[Place, Rect[MeasurementUnit], Rect[MeasurementUnit], Rect[BoundUnit], Collection, Point2d[MeasurementUnit]],
  childSize : Widget => Rect[MeasurementUnit],
) : Place[Sized[MeasurementUnit, Collection[(Widget, Point2d[MeasurementUnit])]]] =
  for
    sizedItems <- measureItemsOneByOne[Place, Collection, Rect[BoundUnit], Widget](
      getBounds = getBounds,
      setBounds = setBounds,
      updateBoundsAccordingToItem = (bounds, item) => 
        bounds.mapAlong(mainAxis, cut(_, childSize(item).along(mainAxis))),
      items = children
    )
    bounds <- getBounds
    placedItems <- elementsPlacement(sizedItems.map(childSize), bounds)
  yield Sized(
    sizedItems.zip(placedItems.coordinates),
    placedItems.size
  )
end rowColumnLayoutPlacement

/**
 * По очереди измеряет размеры виджетов, изменяя количество свободного пространства в соответствии с их размерами.
 * После своей работы оставляет количество свободного пространства неизменным.
 *
 * @param getBounds Возвращает ограничения на размеры виджета
 * @param setBounds Устанавливает ограничения на размеры виджета
 * @param updateBoundsAccordingToItem Вычисляет новые ограничения на размеры виджета, исходя из текущих и нового установленного виджета
 */
def measureItemsOneByOne[
  Measure[_] : Monad,
  Collection[_] : Traverse,
  Bounds,
  Item,
](
  getBounds : Measure[Bounds],
  setBounds : Bounds => Measure[Unit],
  updateBoundsAccordingToItem : (Bounds, Item) => Bounds,
  items : Collection[Measure[Item]],
) : Measure[Collection[Item]] =
  for
    initialBounds <- getBounds
    res <- measureItemsDirty[Measure, Collection, Item](
        item => update(getBounds, setBounds)(updateBoundsAccordingToItem(_, item)),
        items,
    )
    _ <- setBounds(initialBounds)
  yield res
end measureItemsOneByOne

/**
 * Измеряет размеры виджетов, изменяя количество свободного пространства в соответствии с их размерами.
 * После своей работы оставляет количество свободного пространства измененным.
 *
 * @param updateBounds Обновляет ограничения на размеры виджета
 */
def measureItemsDirty[
  Measure[_] : Monad,
  Collection[_] : Traverse,
  Item
](
  updateBounds : Item => Measure[Unit],
  items : Collection[Measure[Item]]
) : Measure[Collection[Item]] =
  items.traverse(current =>
    for
      currentItem <- current
      _ <- updateBounds(currentItem)
    yield  currentItem
  )
end measureItemsDirty
