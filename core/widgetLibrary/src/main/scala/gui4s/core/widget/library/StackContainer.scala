package gui4s.core.widget.library

import catnip.syntax.all.{*, given}
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.*
import cats.syntax.all.*

import gui4s.core.geometry.*
import gui4s.core.layout.*
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
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
 * @tparam Bounds Ограничения на доступное место
 * @tparam Size Единица измерения места на экране
 * @return
 */
def stackContainer[
  Widget,
  PlacementEffect[_] : Monad as OPA,
  Size,
  Bounds,
  MeasurementUnit : Numeric as MUN
](
  getBounds : PlacementEffect[Bounds],
  container : ContainerWidget[
    PlacementEffect[Sized[Size, Widget]],
    List[
      PlacementEffect[Sized[Size, Widget]],
    ],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[Size, Bounds, Widget]],
      Size,
      Bounds,
      List,
      Measured[Size, Bounds, (Widget, Point3d[MeasurementUnit])]
    ]
  ],
  widgetAsFree : AsFreeF[Widget, PlacementEffect * SizedC[Size]]
)(
   children : List[PlacementEffect[Sized[Size, Widget]]],
   xyPlacementStrategy : PlacementStrategy[PlacementEffect, Size, Size, Bounds, List, Point2d[MeasurementUnit]],
) : PlacementEffect[Sized[Size, Widget]] =
  given Functor[PlacementEffect * SizedC[Size]] = nestedFunctorsAreFunctors[PlacementEffect, SizedC[Size]]
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
