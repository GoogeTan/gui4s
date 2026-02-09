package gui4s.core.widget.library

import catnip.Get
import catnip.Set
import catnip.Zip
import catnip.syntax.additional.*
import cats.{Applicative, Monad, Order, Traverse}
import cats.syntax.all.*
import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.Placed
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.OneElementPlacementStrategy
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.layout.rowcolumn.rowColumnLayoutPlacement

/**
 * Линейный контейнер - это контейнер, который располагает свои дочерние виджеты вдоль одной из осей.
 *
 * @param children Множество дочерних виджетов
 * @param mainAxis Ось, вдоль которой располагаются дочерние виджеты
 * @param mainAxisStrategy Функция, размещающая дочерние виджеты вдоль главной оси. Например, размещающая в начале или в конце, заполняющая все место с равным расстоянием между
 * @param additionalAxisStrategy Функция, размещающая виджет вдоль второй оси. Например, в начале, в середине или в конце.
 *
 * @tparam Widget Свободный виджет
 * @tparam Place Эффект устаовки виджета на экран
 * @tparam Collection Множество виджетов
 * @tparam BoundUnit Ограничения на доступное место(может отличастья от MeasurementUnit, т.к. может быть бесконечным в случае скролла)
 * @tparam MeasurementUnit Единица измерения размеров на экране
 * @tparam Axis Ось.
 */
@FunctionalInterface
trait LinearContainer[
  Widget,
  Place[_],
  Collection[_],
  BoundUnit,
  MeasurementUnit,
  Axis,
]:
  /**
   * @param children Множество дочерних виджетов
   * @param mainAxis Ось, вдоль которой располагаются дочерние виджеты
   * @param mainAxisStrategy Функция, размещающая дочерние виджеты вдоль главной оси. Например, размещающая в начале или в конце, заполняющая все место с равным расстоянием между
   * @param additionalAxisStrategy Функция, размещающая виджет вдоль второй оси. Например, в начале, в середине или в конце.
   */
  def apply(
              children               : Collection[Widget],
              mainAxis               : Axis,
              mainAxisStrategy       : PlacementStrategy[Place, MeasurementUnit, BoundUnit, Collection, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, MeasurementUnit, BoundUnit, MeasurementUnit],
            ) : Widget
end LinearContainer

/**
 * Реализация линейного контейнера через обобщенный контейнер.
 * @see  См. определение линейного контейнера [[LinearContainer]]
 *
 * @param container Обобщенный контейнер
 * @param getBounds Возвращает ограничения на доступное место
 * @param setBounds Позволяет установить ограничения на доступное место
 * @param cut Уменьшает ограничения на заданную величину
 *
 * @tparam PlacedWidget Размещенный виджет.
 * @tparam Place Эффект устаовки виджета на экран
 * @tparam Collection Множество виджетов
 * @tparam BoundUnit Ограничения на доступное место(может отличастья от MeasurementUnit, т.к. может быть бесконечным в случае скролла)
 * @tparam MeasurementUnit Единица измерения размеров на экране
 */
def linearContainer[
  PlacedWidget,
  Place[_] : Monad,
  Collection[_] : {Applicative as A, Traverse, Zip},
  BoundUnit,
  MeasurementUnit : Numeric as N,
](
  container : ContainerWidget[PlacedWidget, Collection, Place * Sized[MeasurementUnit, *], Point3d[MeasurementUnit]],
  getBounds: Get[Place, Rect[BoundUnit]],
  setBounds: Set[Place, Rect[BoundUnit]],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit 
) : LinearContainer[Place[Sized[MeasurementUnit, PlacedWidget]], Place, Collection, BoundUnit, MeasurementUnit, Axis] =
  (children, mainAxis, mainAxisStrategy, additionalAxisStrategy) =>
    container(
      children,
      freeChildren =>
        rowColumnLayoutPlacement[
          Place, Collection, PlacedWidget, BoundUnit, MeasurementUnit
        ](
          getBounds,
          setBounds,
          cut,
          mainAxis,
          freeChildren,
          PlacementStrategy.Zip(
            mainAxis,
            mainAxisStrategy,
            PlacementStrategy.PlaceListIndependently(additionalAxisStrategy)
          ),
        ).map(_.mapValue(elements => A.map(elements)(placedElementAsLayoutMetadata)))
    )
end linearContainer

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, Point3d[MeasurementUnit]) =
  (placed.value, placed.coordinate)
end placedElementAsLayoutMetadata
