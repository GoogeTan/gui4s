package gui4s.core.widget.library

import catnip.syntax.all.given
import cats.Monad
import cats.syntax.all.*
import gui4s.core.geometry.{Axis, Point3d, Rect}
import gui4s.core.layout.*

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
              mainAxisStrategy       : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundUnit, Collection, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundUnit, MeasurementUnit],
            ) : Widget
end WeightedLinearContainer

/**
 * Реализация взвешенного линейного контейнера через обобщенный контейнер.
 */
def weightedLinearContainer[
  PlacedWidget,
  PlacementEffect[_] : Monad,
  BoundUnit,
  MeasurementUnit : Numeric as N,
](
  container : ContainerWidget[
    PlacementEffect[Sized[Rect[MeasurementUnit], PlacedWidget]],
    List[Weighted[PlacementEffect[Sized[Rect[MeasurementUnit], PlacedWidget]]]],
    PlacementStrategy[
      PlacementEffect,
      Weighted[PlacementEffect[Measured[Rect[MeasurementUnit], Rect[BoundUnit], PlacedWidget]]],
      Rect[MeasurementUnit],
      Rect[BoundUnit],
      List,
      (PlacedWidget, Measured[Rect[MeasurementUnit], Rect[BoundUnit], (Option[Float], Point3d[MeasurementUnit])])
    ]
  ],
  getBounds: PlacementEffect[Rect[BoundUnit]],
  withBounds: (Rect[BoundUnit], PlacementEffect[Measured[Rect[MeasurementUnit], Rect[BoundUnit], PlacedWidget]]) =>
    PlacementEffect[Measured[Rect[MeasurementUnit], Rect[BoundUnit], PlacedWidget]],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  weightedBounds : (BoundUnit, Float) => BoundUnit,
) : WeightedLinearContainer[PlacementEffect[Sized[Rect[MeasurementUnit], PlacedWidget]], PlacementEffect, List, BoundUnit, MeasurementUnit, Axis] =
  (children, mainAxis, mainAxisStrategy, additionalAxisStrategy) =>
    container(
      children,
      ContainerStrategy.combine(
        measurementStrategy = MeasurementStrategy.weightedLinearMeasurementStrategy(
          getBounds,
          withBounds,
          cut,
          weightedBounds,
          mainAxis
        ),
        placementStrategy = PlacementStrategy.Zip(
          mainAxis,
          mainAxisStrategy,
          PlacementStrategy.PlaceListIndependently(additionalAxisStrategy)
        ),
        someMap = _.mapWithIndex:
          case ((measuredWidget, point), index) =>
            (
              measuredWidget.value,
              Measured(
                (None, new Point3d(point, N.fromInt(index))),
                measuredWidget.size,
                measuredWidget.bounds
              )
            ),
        sizeOfItem = _.size
      )
    )
end weightedLinearContainer