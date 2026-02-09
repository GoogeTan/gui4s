package gui4s.core.widget.library

import catnip.{Get, Set}
import catnip.syntax.all.{*, given}
import cats.{Monad, Order}
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, Point3d, Rect}
import gui4s.core.layout.{Sized, Weighted}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy, weightedRowColumnLayoutPlacement}

/**
 * Взвешенный линейный контейнер - это линейный контейнер, некоторые элементы которого имеют вес.
 * Их размер рассчитывается после размера всех остальных элементов. Каждому предоставляется свободное место
 * пропорционально его весу.
 *
 * @param children Множество дочерних виджетов с весами
 * @param mainAxis Ось, вдоль которой располагаются дочерние виджеты
 * @param additionalAxisStrategy Функция, размещающая виджет вдоль второй оси.
 *
 * @tparam Widget Свободный виджет
 * @tparam Place Эффект устаовки виджета на экран
 * @tparam Collection Множество виджетов
 * @tparam BoundUnit Ограничения на доступное место
 * @tparam MeasurementUnit Единица измерения размеров на экране
 * @tparam Axis Ось.
 */
@FunctionalInterface
trait WeightedLinearContainer[
  Widget,
  Place[_],
  Collection[_],
  BoundUnit,
  MeasurementUnit,
  Axis,
]:
  def apply(
              children               : Collection[Weighted[Widget]],
              mainAxis               : Axis,
              mainAxisStrategy       : PlacementStrategy[Place, MeasurementUnit, BoundUnit, Collection, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, MeasurementUnit, BoundUnit, MeasurementUnit],
            ) : Widget
end WeightedLinearContainer

/**
 * Реализация взвешенного линейного контейнера через обобщенный контейнер.
 */
def weightedLinearContainer[
  PlacedWidget,
  Place[_] : Monad,
  BoundUnit,
  MeasurementUnit : Numeric as N,
](
  container : ContainerWidget[PlacedWidget, List, Place * Sized[MeasurementUnit, *], Point3d[MeasurementUnit]],
  getBounds: Get[Place, Rect[BoundUnit]],
  setBounds: Set[Place, Rect[BoundUnit]],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  weightedBounds : (BoundUnit, Float) => BoundUnit,
  toMeasurementUnit : BoundUnit => Option[MeasurementUnit]
) : WeightedLinearContainer[Place[Sized[MeasurementUnit, PlacedWidget]], Place, List, BoundUnit, MeasurementUnit, Axis] =
  (children, mainAxis, mainAxisStrategy, additionalAxisStrategy) =>
    container(
      children.map(_.value),
      freeChildren =>
        weightedRowColumnLayoutPlacement(
          getBounds,
          setBounds,
          cut,
          weightedBounds,
          mainAxis,
          freeChildren.zip(children).map((free, weights) => weights.as(free)),//TODO remove this shit and allow so store additional data in container widget.
          PlacementStrategy.Zip[Place, MeasurementUnit, BoundUnit, List, MeasurementUnit](
            mainAxis,
            mainAxisStrategy,
            PlacementStrategy.PlaceListIndependently(additionalAxisStrategy)
          )
        ).map(_.mapValue(_.map(placedElementAsLayoutMetadata)))
    )
end weightedLinearContainer