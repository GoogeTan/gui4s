package gui4s.core.widget.library

import catnip.Get
import catnip.Set
import catnip.Zip
import catnip.syntax.additional._
import cats.Monad
import cats.Traverse
import cats.syntax.all._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout._
import gui4s.core.widget.free.AsFreeF

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
  MeasurementUnit,
  BoundUnit,
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
              mainAxisStrategy       : PlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundUnit, Collection, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, MeasurementUnit, MeasurementUnit, BoundUnit, MeasurementUnit],
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
 * @tparam Widget Размещенный виджет.
 * @tparam PlacementEffect Эффект устаовки виджета на экран
 * @tparam Collection Множество виджетов
 * @tparam BoundUnit Ограничения на доступное место(может отличастья от MeasurementUnit, т.к. может быть бесконечным в случае скролла)
 * @tparam MeasurementUnit Единица измерения размеров на экране
 */
def linearContainer[
  Widget,
  PlacementEffect[_] : Monad,
  Collection[_] : {Traverse, Zip},
  MeasurementUnit : Numeric as N,
  BoundUnit,
](
  container : ContainerWidget[
    PlacementEffect[Sized[Rect[MeasurementUnit], Widget]],
    Collection[
      PlacementEffect[Sized[Rect[MeasurementUnit], Widget]],
    ],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[Rect[MeasurementUnit], Rect[BoundUnit], Widget]],
      Rect[MeasurementUnit],
      Rect[BoundUnit],
      Collection,
      Measured[Rect[MeasurementUnit], Rect[BoundUnit], (Widget, Point3d[MeasurementUnit])]
    ]
  ],
  getBounds: Get[PlacementEffect, Rect[BoundUnit]],
  setBounds: Set[PlacementEffect, Rect[BoundUnit]],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit,
  widgetAsFree : AsFreeF[Widget, PlacementEffect * Sized[Rect[MeasurementUnit], *]]
) : LinearContainer[PlacementEffect[Sized[Rect[MeasurementUnit], Widget]], PlacementEffect, Collection, MeasurementUnit, BoundUnit, Axis] =
  (children, mainAxis, mainAxisStrategy, additionalAxisStrategy) =>
    container(
      children,
      ContainerStrategy.combine(
        measurementStrategy = MeasurementStrategy.linearMeasurementStrategy[
          PlacementEffect,
          Collection,
          Rect[BoundUnit],
          Measured[Rect[MeasurementUnit], Rect[BoundUnit], Widget]
        ](
          getBounds,
          setBounds,
          (bounds, item) => bounds.mapAlong(mainAxis, cut(_, item.size.along(mainAxis))) 
        ),
        placementStrategy = PlacementStrategy.Zip(
          mainAxis,
          mainAxisStrategy,
          PlacementStrategy.PlaceListIndependently(additionalAxisStrategy)
        ),
        someMap = _.mapWithIndex:
          case ((measuredWidget, point), index) =>
            measuredWidget.map(widget => (widget, new Point3d(point, N.fromInt(index)))),
        sizeOfItem = _.size
      )
    )
end linearContainer