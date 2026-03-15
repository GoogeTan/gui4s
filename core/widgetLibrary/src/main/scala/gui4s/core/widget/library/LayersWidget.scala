package gui4s.core.widget.library

import catnip.syntax.additional._
import catnip.syntax.all.given
import cats._
import cats.syntax.all._

import gui4s.core.geometry._
import gui4s.core.layout._
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
 * @tparam Size Единица измерения размеров на экране.
 */
def layersWidget[
  Widget,
  PlacementEffect[_] : Monad as OPA,
  Size,
  Bounds,
  MeasurementUnit : Numeric as MUN
](
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
  withBounds : Size => PlacementEffect ~> PlacementEffect,
)(
   background : List[PlacementEffect[Sized[Size, Widget]]],
   foreground : List[PlacementEffect[Sized[Size, Widget]]],
   decorationsPlacementStrategy : PlacementStrategy[
     PlacementEffect,
     Size,
     Size,
     Size,
     List,
     Point2d[MeasurementUnit]
   ],
   masterPlacementStrategy : OneElementPlacementStrategy[
     PlacementEffect,
     Size,
     Size,
     Bounds,
     Point2d[MeasurementUnit]
   ],
) : Decorator[
  PlacementEffect[Sized[Size, Widget]]
] =
  original =>
    container(
      background ++ (original :: foreground),
      ContainerStrategy.combine(
        measurementStrategy = MeasurementStrategy.layeredPlace[
          PlacementEffect,
          Measured[Size, Bounds, Widget]
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
  Size,
  Bounds,
  Point
](
  getBounds : PlacementEffect[Bounds],
  value : PlacementEffect[Sized[Size, Widget]]
) : PlacementEffect[Measured[Size, Bounds, Widget]] =
  getBounds.flatMap(bounds =>
    value.map(new Measured(_, bounds))
  )
end measure

def measureIncrementally[
  PlacementEffect[_] : Monad,
  Widget,
  Size,
  Bounds,
  Point
](
  getBounds : PlacementEffect[Bounds],
  widgetAsFree : AsFreeF[Widget, PlacementEffect * SizedC[Size]],
  widget: Measured[Size, Bounds, Widget]
): PlacementEffect[Sized[Size, Widget]] =
  getBounds.flatMap:
    bounds =>
      if widget.bounds == bounds then
        Sized(widget.value, widget.size).pure[PlacementEffect]
      else
        widgetAsFree(widget.value)
end measureIncrementally