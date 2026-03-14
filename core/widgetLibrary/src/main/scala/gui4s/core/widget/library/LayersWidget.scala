package gui4s.core.widget.library

import catnip.syntax.additional.*
import catnip.syntax.all.given
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.*
import gui4s.core.widget.free.AsFreeF
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
  MeasurementUnit : Numeric as MUN,
  BoundUnit
](
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
  withBounds : Rect[MeasurementUnit] => PlacementEffect ~> PlacementEffect,
)(
   background : List[PlacementEffect[Sized[MeasurementUnit, Widget]]],
   foreground : List[PlacementEffect[Sized[MeasurementUnit, Widget]]],
   decorationsPlacementStrategy : PlacementStrategy[
     PlacementEffect,
     Rect[MeasurementUnit],
     Rect[MeasurementUnit],
     Rect[MeasurementUnit],
     List,
     Point2d[MeasurementUnit]
   ],
   masterPlacementStrategy : OneElementPlacementStrategy[
     PlacementEffect,
     Rect[MeasurementUnit],
     Rect[MeasurementUnit],
     Rect[BoundUnit],
     Point2d[MeasurementUnit]
   ],
) : Decorator[
  PlacementEffect[Sized[MeasurementUnit, Widget]]
] =
  original =>
    container(
      background ++ (original :: foreground),
      ContainerStrategy.combine(
        measurementStrategy = MeasurementStrategy.layeredPlace[
          PlacementEffect,
          Measured[MeasurementUnit, BoundUnit, Widget]
        ](
          background.size,
          (masterWidget, currentWidget) =>
            withBounds(masterWidget.size)(currentWidget)
        ),
        placementStrategy = PlacementStrategy.MasterBasedStack(
          decorationsPlacementStrategy,
          masterPlacementStrategy,
          background.size
        ),
        someMap = _.mapWithIndex:
          case ((measuredWidget, point), index) =>
            Measured(
              (
                measuredWidget.value,
                new Point3d(point, MUN.fromInt(index))
              ),
              measuredWidget.size,
              measuredWidget.bounds,
            ),
        sizeOfItem = _.size
      )
    )
end layersWidget

def measure[
  PlacementEffect[_] : FlatMap,
  Widget,
  MeasurementUnit,
  BoundUnit,
  Point
](
  getBounds : PlacementEffect[Rect[BoundUnit]],
  value : PlacementEffect[Sized[MeasurementUnit, Widget]]
) : PlacementEffect[Measured[MeasurementUnit, BoundUnit, Widget]] =
  getBounds.flatMap(bounds =>
    value.map(new Measured(_, bounds))
  )
end measure

def measureIncrementally[
  PlacementEffect[_] : Monad,
  Widget,
  MeasurementUnit,
  BoundUnit,
  Point
](
  getBounds : PlacementEffect[Rect[BoundUnit]],
  widgetAsFree : AsFreeF[Widget, PlacementEffect * SizedC[MeasurementUnit]],
  widget: Measured[MeasurementUnit, BoundUnit, Widget]
): PlacementEffect[Sized[MeasurementUnit, Widget]] =
  getBounds.flatMap:
    bounds =>
      if widget.bounds == bounds then
        Sized(widget.value, widget.size).pure[PlacementEffect]
      else
        widgetAsFree(widget.value)
end measureIncrementally