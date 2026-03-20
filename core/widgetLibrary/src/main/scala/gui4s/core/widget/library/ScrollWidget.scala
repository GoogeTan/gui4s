package gui4s.core.widget.library

import catnip.syntax.all.given
import cats.*
import cats.syntax.all.*
import gui4s.core.layout.{ContainerStrategy, Measured, MeasurementStrategy, PlacementStrategy, Sized}
import gui4s.core.widget.library.decorator.Decorator

/**
 * Виджет прокрутки. Позволяет просматривать невидимые части дочернего виджета посредством прокрутки.
 * Также дочерний виджет получит бесконечные ограничения по одной или нескольким осям прокрутки.
 * @param containerWidget Одноместный контейнер, нужный, чтобы измерять размер ребенка с учетом бесконечных ограничений.
 * @param scrolledShiftAndClip Декоратор, обрезающий части дочернго виджета, выходящие за рамки, и предоставляющий нынешнее количество прокрутки
 * @param withInfiniteBounds Вычисляет эффект установки с использованием бесконечных ограничений по нужным осям
 * @param placementStrategy Устанавливает дочерний виджет внутри имеющегося места. Важно знать, что место предоставляется не с бесконечными ограничениями. Учесть это - задача вызывающей стороны
 * @param body Дочерний виджет
 * @param combinePoints Функция, сдвигающая положение дочернего виджета на имеющийся сдвиг
 * @tparam Widget Тип свободного виджета
 * @tparam SizedWidget Тип установленного виджета
 * @tparam Size Размер установленного виджета
 * @tparam Bounds Ограничения на доступное место
 * @tparam Point Точка, в которую был установлен виджет
 * @tparam Shift Сдвиг прокрутки
 */
def scrollWidget[
  Widget,
  SizedWidget,
  PlacementEffect[_] : FlatMap,
  Size,
  Bounds,
  Point,
  Shift
](
  containerWidget: ContainerWidget[
    Widget,
    Widget,
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[Size, Bounds, SizedWidget]],
      Size,
      Bounds,
      Id,
      Measured[Size, Bounds, (SizedWidget, Point)]
    ]
  ],
  scrolledShiftAndClip : (Shift => Widget) => Widget,
  withInfiniteBounds : PlacementEffect ~> PlacementEffect,
  placementStrategy: PlacementStrategy[PlacementEffect, Size, Size, Bounds, Id, Point],
  body : Widget,
  combinePoints : (Point, Shift) => Point
) : Widget =
  scrolledShiftAndClip(scrollAmount =>
    containerWidget(
      body,
      ContainerStrategy.combine(
        measurementStrategy = withInfiniteBounds(_),
        placementStrategy = placementStrategy,
        someMap = {
          case (Measured(widget, size, bounds), point: Point) =>
            Measured((widget, combinePoints(point, scrollAmount)), size, bounds)
        },
        sizeOfItem = _.size
      )
    )
  )
end scrollWidget
