package gui4s.core.layout
package rowcolumn

import catnip.Set._
import catnip.Zip
import catnip.Zip.zip
import cats._
import cats.syntax.all._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect

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
  Container[_] : {Traverse, Zip},
  Widget,
  BoundUnit,
  MeasurementUnit : Numeric as MUN,
](
  getBounds: Place[Rect[BoundUnit]],
  setBounds: Rect[BoundUnit] => Place[Unit],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  mainAxis : Axis,
  children : Container[Place[Sized[MeasurementUnit, Widget]]],
  elementsPlacement : PlacementStrategy[Place, Rect[BoundUnit], Container, Point2d[MeasurementUnit]],
) : Place[Sized[MeasurementUnit, Container[Placed[MeasurementUnit, Widget]]]] =
  for
    initialBounds <- getBounds
    sizedItems <- measureItemsDirty[Place, Container, Sized[MeasurementUnit, Widget]](
      item => update(getBounds, setBounds)(_.mapAlong(mainAxis, cut(_, item.lengthAlong(mainAxis)))),
      children,
    )
    _ <- setBounds(initialBounds)
    placedItems <- elementsPlacement(sizedItems.map(_.size.toPoint2d), initialBounds)
  yield Sized(
    sizedItems.zip(placedItems.coordinatesOfStarts).map(new Placed(_, _, MUN.zero)),
    new Rect(placedItems.coordinateOfEnd)
  )
end rowColumnLayoutPlacement

/**
 * Измеряет размеры виджетов, изменяя количество свободного пространства в соответствии с их размерами.
 * После своей работы оставляет количество свободного пространства измененным.
 *
 * @param updateBounds Обновляет ограничения на размеры виджета
 */
def measureItemsDirty[Measure[_] : Monad, Container[_] : Traverse, Item](updateBounds : Item => Measure[Unit], items : Container[Measure[Item]]) : Measure[Container[Item]] =
  items.traverse(current =>
    for
      currentItem <- current
      _ <- updateBounds(currentItem)
    yield  currentItem
  )
end measureItemsDirty
