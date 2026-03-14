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
import gui4s.core.widget.library.{GenericLayout, Layout, LayoutIncrementalWidget}
import gui4s.desktop.widget.library.{Layout2, widgetAsFree, widgetHandlesEvent, widgetIsDrawable, container as genericContainer}
import gui4s.core.widget.library.*


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
    Measured[Float, InfinityOr[Float], (AndroidPlacedWidget[Event], Point3d[Float])]
  ]
) : AndroidWidget[Event] =
  given Order[Point3d[Float]] = Order.by(_.z)
  containerWidget2[
    Collection,
    Event
  ](
    children,
    freeChildren =>
      for
        bounds <- PlacementEffect.getBounds
        placed <- placementStrategy(freeChildren.map(_.map(new Measured(_, bounds))), bounds)
      yield Sized(placed.coordinates, placed.size)
  )
end containerWidget

def containerWidget2[
  Collection[_] : {Traverse, Sortable, Zip},
  Event
](
  childrenIn : Collection[AndroidWidget[Event]],
  layout : Layout2[
    Place,
    Collection,
    AndroidPlacedWidget[Event],
    Measured[Float, InfinityOr[Float], (AndroidPlacedWidget[Event], Point3d[Float])]
  ],
) : AndroidWidget[Event] =
  type WidgetAndItsPositionInContainer = Measured[Float, InfinityOr[Float], (AndroidPlacedWidget[Event], Point3d[Float])]
  genericContainer[
    UpdateC[Event],
    Place,
    Collection,
    Draw,
    RecompositionReaction,
    DownEvent,
    WidgetAndItsPositionInContainer
  ](
    positionedChildHandlesEvent = { case (Measured((widget, position), _, _), path, event) =>
      Update.withCornerCoordinates(widgetHandlesEvent(widget, path, event), _ + position)
    },
    isEventConsumed = Update.isEventHandled,
    updateContainerOrdered = children => updateFunction =>
      //Обновляются сначала виджеты, лежащие выше.
      given Order[WidgetAndItsPositionInContainer] = Order.reverse(Order.by(_.value._2.z))
      traverseOrdered[
        UpdateC[Event],
        Collection,
        WidgetAndItsPositionInContainer
      ](children)(updateFunction),
    drawOrdered = children =>
      // А рисуются вначале виджеты, лежащие ниже.
      given Order[WidgetAndItsPositionInContainer] = Order.by(_.value._2.z)
      foldOrdered[Draw, Collection](children)(widget =>
        val point = widget.value._2
        drawAt(point.x, point.y, widgetIsDrawable(widget.value._1))
      ),
    children = childrenIn,
    layout = layout,
    incrementalFreeChildrenFromPlaced =
      (oldWidget, newWidget) =>
        newWidget.getOrElse(
          measureIncrementally(PlacementEffect.getBounds, widgetAsFree, oldWidget.map(_._1))
        ),
    unplaceWidget = _.value._1
  )
end containerWidget2

def oneElementContainerWidget[Event](
                                      child : AndroidWidget[Event],
                                      placementStrategy : PlacementStrategy[
                                        PlacementEffect,
                                        PlacementEffect[Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]]],
                                        Rect[Float],
                                        Bounds,
                                        Id,
                                        Measured[Float, InfinityOr[Float], (AndroidPlacedWidget[Event], Point3d[Float])]
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
    PlacementEffect,
    PlacementEffect[Measured[Float, InfinityOr[Float], AndroidPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    List,
    Measured[Float, InfinityOr[Float], (AndroidPlacedWidget[Event], Point3d[Float])]
  ]
) : AndroidWidget[Event]  =
  containerWidget[List, Event](
    children,
    placementStrategy
  )
end listContainerWidget
