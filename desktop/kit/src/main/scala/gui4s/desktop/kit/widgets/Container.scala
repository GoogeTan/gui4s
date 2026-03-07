package gui4s.desktop.kit
package widgets

import catnip.syntax.all.{*, given}
import catnip.{Sortable, Zip}
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.{InfinityOr, Point3d, Rect}
import gui4s.core.layout.{Measured, OneElementPlacementStrategy, PlacementStrategy, Sized}
import gui4s.core.widget.handle.{Layout, LayoutIncrementalWidget}
import gui4s.core.widget.library.{GenericLayout, LayersMetadata}
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.{monoidInstance, given}
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.{widgetIsDrawable, container as genericContainer}

def measureWithBounds[T](value : Place[T]) : PlacementEffect[Measured[Float, InfinityOr[Float], T]] =
  PlacementEffect.getBounds.flatMap(bounds =>
    value.map(new Measured(_, bounds))
  )
end measureWithBounds

def measureWithBoundsIncrementally[Event, Point](
  child  : LayoutIncrementalWidget[
    DesktopPlacedWidget[Event],
    Place,
    LayersMetadata[Point, Rect[Float], Bounds]
  ]
) : PlacementEffect[
  Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]]
] =
  PlacementEffect.getBounds.flatMap(bounds =>
    child.newWidget.getOrElse[DesktopWidget[Event]](
      if bounds == child.meta.bounds then
        Sized(child.widget, child.meta.size).pure[PlacementEffect]
      else
        gui4s.desktop.widget.library.widgetAsFree(child.widget)
    ).map(new Measured(_, bounds))
  )
end measureWithBoundsIncrementally

def containerWidget[
  Collection[_] : {Traverse, Sortable, Zip},
  Event
](
  children : Collection[DesktopWidget[Event]],
  placementStrategy : PlacementStrategy[
    PlacementEffect,
    PlacementEffect[Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    Collection,
    (DesktopPlacedWidget[Event], LayersMetadata[Point3d[Float], Rect[Float], Bounds])
  ]
) : DesktopWidget[Event] =
  containerWidget2(
    children,
    GenericLayout(
      measureWithBounds = measureWithBounds,
      measureWithBoundsIncrementally = measureWithBoundsIncrementally[Event, Point3d[Float]],
      getBounds = PlacementEffect.getBounds,
      placementStrategy = placementStrategy,
    ),
    incrementalFreeChildrenFromPlaced = LayoutIncrementalWidget(_, _),
    pointOfMeta = _.point
  )
end containerWidget

def containerWidget2[
  Collection[_] : {Traverse, Sortable, Zip},
  Event,
  FreeWidget,
  IncrementalFreeWidget,
  Meta
](
  childrenIn : Collection[FreeWidget],
  layout : Layout[
    Place,
    Collection,
    FreeWidget,
    IncrementalFreeWidget,
    (DesktopPlacedWidget[Event], Meta)
  ],
  incrementalFreeChildrenFromPlaced: (
    (DesktopPlacedWidget[Event], Meta),
    Option[DesktopWidget[Event]]
  ) => IncrementalFreeWidget,
  pointOfMeta : Meta => Point3d[Float]
) : DesktopWidget[Event] =
  type WidgetAndItsPositionInContainer = (DesktopPlacedWidget[Event], Meta)
  genericContainer[
    UpdateC[Event],
    Place,
    Collection,
    Draw,
    RecompositionReaction,
    DownEvent,
    Meta,
    FreeWidget,
    IncrementalFreeWidget
  ](
    [T] => (update, meta) =>
      Update.withCornerCoordinates(update, _ + pointOfMeta(meta)),
    Update.isEventHandled,
    children => updateFunction =>
      //Обновляются сначала виджеты, лежащие выше.
      given Order[WidgetAndItsPositionInContainer] = Order.reverse(Order.by((_, meta) => pointOfMeta(meta).z))
      traverseOrdered[
        UpdateC[Event],
        Collection,
        WidgetAndItsPositionInContainer
      ](children)(updateFunction),
    children =>
      // А рисуются вначале виджеты, лежащие ниже.
      given Order[WidgetAndItsPositionInContainer] = Order.by((_, meta) => pointOfMeta(meta).z)
      foldOrdered[Draw, Collection](children)((widget, meta) =>
        val point = pointOfMeta(meta)
        drawAt(point.x, point.y, widgetIsDrawable(widget))
      ),
    childrenIn,
    layout,
    incrementalFreeChildrenFromPlaced
  )
end containerWidget2

def oneElementContainerWidget[Event](
                                      child : DesktopWidget[Event],
                                      placementStrategy : OneElementPlacementStrategy[
                                        PlacementEffect,
                                        PlacementEffect[Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]]],
                                        Rect[Float],
                                        Bounds,
                                        (DesktopPlacedWidget[Event], LayersMetadata[Point3d[Float], Rect[Float], Bounds])
                                      ]
                                    ) : DesktopWidget[Event]  =
  containerWidget[Id, Event](
    child,
    placementStrategy
  )
end oneElementContainerWidget

def listContainerWidget[
  Event
](
  children : List[DesktopWidget[Event]],
  placementStrategy : PlacementStrategy[
    PlacementEffect,
    PlacementEffect[Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    List,
    (DesktopPlacedWidget[Event], LayersMetadata[Point3d[Float], Rect[Float], Bounds])
  ]
) : DesktopWidget[Event]  =
  containerWidget[List, Event](
    children,
    placementStrategy
  )
end listContainerWidget
