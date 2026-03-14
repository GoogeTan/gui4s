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
import gui4s.core.widget.library.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.{monoidInstance, given}
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.{Layout2, widgetAsFree, widgetHandlesEvent, widgetIsDrawable, container as genericContainer}


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
    Measured[Float, InfinityOr[Float], (DesktopPlacedWidget[Event], Point3d[Float])]
  ]
) : DesktopWidget[Event] =
  given Order[Point3d[Float]] = Order.by(_.z)
  containerWidget2[
    Collection,
    Event
  ](
    children,
    currentChildren =>
      for
        bounds <- PlacementEffect.getBounds
        measuredChildren = currentChildren.map(_.map(new Measured(_, bounds)))
        res <- placementStrategy(measuredChildren, bounds)
      yield Sized(res.coordinates, res.size)
  )
end containerWidget

def containerWidget2[
  Collection[_] : {Traverse, Sortable, Zip},
  Event
](
  childrenIn : Collection[DesktopWidget[Event]],
  layout : Layout2[
    Place,
    Collection,
    DesktopPlacedWidget[Event],
    Measured[Float, InfinityOr[Float], (DesktopPlacedWidget[Event], Point3d[Float])]
  ],
) : DesktopWidget[Event] =
  type WidgetAndItsPositionInContainer = Measured[Float, InfinityOr[Float], (DesktopPlacedWidget[Event], Point3d[Float])]
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
                                      child : DesktopWidget[Event],
                                      placementStrategy : OneElementPlacementStrategy[
                                        PlacementEffect,
                                        PlacementEffect[Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]]],
                                        Rect[Float],
                                        Bounds,
                                        Measured[Float, InfinityOr[Float], (DesktopPlacedWidget[Event], Point3d[Float])]
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
    Measured[Float, InfinityOr[Float], (DesktopPlacedWidget[Event], Point3d[Float])]
  ]
) : DesktopWidget[Event]  =
  containerWidget[List, Event](
    children,
    placementStrategy
  )
end listContainerWidget
