package gui4s.android.kit
package widgets

import catnip.syntax.all.{*, given}
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.{monoidInstance, given}
import gui4s.android.kit.effects.Place.given
import gui4s.android.kit.widgets.{AndroidPlacedWidget, AndroidWidget}
import gui4s.android.skia.canvas.drawAt
import gui4s.core.geometry.{InfinityOr, Point3d, Rect}
import gui4s.core.layout.{Measured, OneElementPlacementStrategy, PlacementStrategy, Sized}
import gui4s.core.widget.handle.{Layout, LayoutIncrementalWidget}
import gui4s.core.widget.library.{GenericLayout, LayersMetadata}
import gui4s.desktop.widget.library.{widgetAsFree, widgetIsDrawable, container as genericContainer}

def measureWithBounds[T](value : Place[T]) : PlacementEffect[Measured[Float, InfinityOr[Float], T]] =
  PlacementEffect.getBounds.flatMap(bounds =>
    value.map(new Measured(_, bounds))
  )
end measureWithBounds

def measureWithBoundsIncrementally[Event, Point](
  child  : LayoutIncrementalWidget[
    AndroidPlacedWidget[Event],
    PlaceC,
    LayersMetadata[Point, Rect[Float], Bounds]
  ]
) : PlacementEffect[
  Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]]
] =
  PlacementEffect.getBounds.flatMap(bounds =>
    child.newWidget.getOrElse[AndroidWidget[Event]](
      if bounds == child.meta.bounds then
        Sized(child.widget, child.meta.size).pure[PlacementEffectC]
      else
        widgetAsFree(child.widget)
    ).map(new Measured(_, bounds))
  )
end measureWithBoundsIncrementally

def containerWidget[
  Collection[_] : {Traverse, Sortable, Zip},
  Event
](
  children : Collection[AndroidWidget[Event]],
  placementStrategy : PlacementStrategy[
    PlacementEffect,
    PlacementEffect[Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    Collection,
    (AndroidPlacedWidget[Event], LayersMetadata[Point3d[Float], Rect[Float], Bounds])
  ]
) : AndroidWidget[Event] =
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
    (AndroidPlacedWidget[Event], Meta)
  ],
  incrementalFreeChildrenFromPlaced: (
    (AndroidPlacedWidget[Event], Meta),
      Option[AndroidWidget[Event]]
    ) => IncrementalFreeWidget,
  pointOfMeta : Meta => Point3d[Float]
) : AndroidWidget[Event] =
  type WidgetAndItsPositionInContainer = (AndroidPlacedWidget[Event], Meta)
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
                                      child : AndroidWidget[Event],
                                      placementStrategy : OneElementPlacementStrategy[
                                        PlacementEffectC,
                                        PlacementEffect[Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]]],
                                        Rect[Float],
                                        Bounds,
                                        (AndroidPlacedWidget[Event], LayersMetadata[Point3d[Float], Rect[Float], Bounds])
                                      ]
                                    ) : AndroidWidget[Event]  =
  containerWidget[Id, Event](
    child,
    placementStrategy
  )
end oneElementContainerWidget

def listContainerWidget[
  Event
](
  children : List[AndroidWidget[Event]],
  placementStrategy : PlacementStrategy[
    PlacementEffectC,
    PlacementEffect[Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    List,
    (AndroidPlacedWidget[Event], LayersMetadata[Point3d[Float], Rect[Float], Bounds])
  ]
) : AndroidWidget[Event]  =
  containerWidget[List, Event](
    children,
    placementStrategy
  )
end listContainerWidget
