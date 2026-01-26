package gui4s.core.widget.library

import catnip.syntax.additional._
import cats._
import cats.syntax.all._

import gui4s.core.geometry._
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

/**
 * Декоратор, которй позволяет добавить виджеты на задный и передний план данного виджета.
 *
 * @param container Обобщенный виджет контейнер
 * @param background Виджеты, которые будут отображаться на задном плане
 * @param foreground Виджеты, которые будут отображаться на переднем плане
 * @param decorationsPlacementStrategy То, как разместить виджеты
 * @tparam Widget Свободный виджет
 * @tparam PlacementEffect Эффект установки на экран
 * @tparam MeasurementUnit Единица измерения размеров на экране.
 */
def layersWidget[
  Widget,
  PlacementEffect[_] : Monad as OPA,
  MeasurementUnit : Numeric as MUN
](
  container : ContainerWidget[
    Widget,
    List,
    PlacementEffect * SizedC[MeasurementUnit],
    Point3d[MeasurementUnit]
  ]
)(
   background : List[PlacementEffect[Sized[MeasurementUnit, Widget]]],
   foreground : List[PlacementEffect[Sized[MeasurementUnit, Widget]]],
   decorationsPlacementStrategy : PlacementStrategy[PlacementEffect, Rect[MeasurementUnit], Rect[MeasurementUnit], List, Point2d[MeasurementUnit]],
) : Decorator[
  PlacementEffect[Sized[MeasurementUnit, Widget]]
] =
  original =>
    container(
      background ++ (original :: foreground),
      elements =>
        for
          sizedElements <- elements.traverse(identity)
          originalSized = sizedElements(background.size)
          backgroundSizes = sizedElements.take(background.size).map(_.size)
          foregroundSizes = sizedElements.takeRight(foreground.size).map(_.size)
          placedElements <- decorationsPlacementStrategy(backgroundSizes ++ foregroundSizes, originalSized.size)
          elementsCoodinates = placedElements.coordinates
          shifts = (
            elementsCoodinates.take(background.size)
              ++ (Point2d(MUN.zero, MUN.zero) :: elementsCoodinates.takeRight(foreground.size))
          ).mapWithIndex((point, index) => new Point3d(point, MUN.fromInt(index)))
        yield Sized(sizedElements.map(_.value).zip(shifts), originalSized.size)
    )
end layersWidget
