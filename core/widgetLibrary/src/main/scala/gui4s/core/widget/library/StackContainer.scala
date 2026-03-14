package gui4s.core.widget.library

import catnip.syntax.all.{*, given}
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
import gui4s.core.layout.*
import gui4s.core.widget.free.AsFreeF
import gui4s.core.widget.library.ContainerWidget

/**
 * Стопка - это контейнер, который располгагает свои дочерние виджеты друг над другом. Первый виджет самый нижкний, последний - самый верхний.
 * @param getBounds Функция, возвращающая ограничения на доступное место
 * @param container Обобщенный виджет контейнера
 * @param children Дочерние виджеты
 * @param xyPlacementStrategy Правило расстановки дочерних виджетов
 * @tparam Widget Размещенный виджет
 * @tparam PlacementEffect Эффект установки виджета
 * @tparam BoundUnit Ограничения на доступное место
 * @tparam MeasurementUnit Единица измерения места на экране
 * @return
 */
def stackContainer[
  Widget,
  PlacementEffect[_] : Monad as OPA,
  BoundUnit,
  MeasurementUnit : Numeric as MUN
](
  getBounds : PlacementEffect[Rect[BoundUnit]],
  container : ContainerWidget[
    PlacementEffect[Sized[MeasurementUnit, Widget]],
    List[
      PlacementEffect[Sized[MeasurementUnit, Widget]],
    ],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[MeasurementUnit, BoundUnit, Widget]],
      Rect[MeasurementUnit],
      Rect[BoundUnit],
      List,
      Measured[MeasurementUnit, BoundUnit, (Widget, Point3d[MeasurementUnit])]
    ]
  ],
  widgetAsFree : AsFreeF[Widget, PlacementEffect * SizedC[MeasurementUnit]]
)(
   children : List[PlacementEffect[Sized[MeasurementUnit, Widget]]],
   xyPlacementStrategy : PlacementStrategy[PlacementEffect, Rect[MeasurementUnit], Rect[MeasurementUnit], Rect[BoundUnit], List, Point2d[MeasurementUnit]],
) : PlacementEffect[Sized[MeasurementUnit, Widget]] =
  given Functor[PlacementEffect * SizedC[MeasurementUnit]] = nestedFunctorsAreFunctors[PlacementEffect, SizedC[MeasurementUnit]]
  given Order[(Point2d[MeasurementUnit], Int)] = Order.by(_._2)
  container(
    children,
    ContainerStrategy.combine(
      measurementStrategy = MeasurementStrategy.independentMeasurementStrategy,
      placementStrategy = xyPlacementStrategy,
      someMap = _.mapWithIndex:
        case ((measuredWidget, point), index) =>
          Measured(
            (measuredWidget.value, new Point3d(point, MUN.fromInt(index))),
            measuredWidget.size,
            measuredWidget.bounds
          ),
      sizeOfItem = _.size
    )
  )
end stackContainer
