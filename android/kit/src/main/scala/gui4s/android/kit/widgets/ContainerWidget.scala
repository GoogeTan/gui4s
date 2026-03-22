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
import gui4s.core.widget.library.{Layout, LayoutIncrementalWidget}
import gui4s.desktop.widget.library.{Layout2, widgetAsFree, widgetHandlesEvent, widgetIsDrawable, container as genericContainer}
import gui4s.core.widget.library.*


def containerWidget[
  Collection[_] : {Traverse, Sortable, Zip},
  Event
](
  children : Collection[AndroidWidget[Event]],
  placementStrategy : PlacementStrategy[
    PlacementEffect,
    PlacementEffect[Measured[Rect[Float], Bounds, AndroidPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    Collection,
    Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])]
  ]
) : AndroidWidget[Event] =
  given Order[Point3d[Float]] = Order.by(_.z)
  containerWidget2[
    Collection,
    Event
  ](
    children,
    freeChildren =>
      PlacementEffect.getBounds.flatMap(bounds =>
        placementStrategy(freeChildren.map(_.map(new Measured(_, bounds))), bounds)
      )
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
    Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])]
  ],
) : AndroidWidget[Event] =
  type WidgetAndItsPositionInContainer = Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])]
  genericContainer[
    UpdateC[Event],
    Place,
    Collection,
    Draw,
    RecompositionReaction,
    WidgetAndItsPositionInContainer
  ](
    positionedChildHandlesEvent = { case (Measured((widget, position), _, _), path) =>
      Update.withCornerCoordinates(widgetHandlesEvent(widget, path), _ + position)
    },
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
                                        PlacementEffect[Measured[Rect[Float], Bounds, AndroidPlacedWidget[Event]]],
                                        Rect[Float],
                                        Bounds,
                                        Id,
                                        Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])]
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
    PlacementEffect[Measured[Rect[Float], Bounds, AndroidPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    List,
    Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])]
  ]
) : AndroidWidget[Event]  =
  containerWidget[List, Event](
    children,
    placementStrategy
  )
end listContainerWidget
