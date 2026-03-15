package gui4s.desktop.kit
package widgets

import catnip.Sortable
import catnip.Zip
import catnip.syntax.all.{_, given}
import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._

import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.Measured
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy
import gui4s.core.layout.Sized
import gui4s.core.widget.library._

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Draw.monoidInstance
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.skija.canvas.drawAt
import gui4s.desktop.widget.library.Layout2
import gui4s.desktop.widget.library.widgetAsFree
import gui4s.desktop.widget.library.widgetHandlesEvent
import gui4s.desktop.widget.library.widgetIsDrawable
import gui4s.desktop.widget.library.{container => genericContainer}


def containerWidget[
  Collection[_] : {Traverse, Sortable, Zip},
  Event
](
  children : Collection[DesktopWidget[Event]],
  placementStrategy : PlacementStrategy[
    PlacementEffect,
    PlacementEffect[Measured[Rect[Float], Bounds, DesktopPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    Collection,
    Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])]
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
    Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])]
  ],
) : DesktopWidget[Event] =
  type WidgetAndItsPositionInContainer = Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])]
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
                                        PlacementEffect[Measured[Rect[Float], Bounds, DesktopPlacedWidget[Event]]],
                                        Rect[Float],
                                        Bounds,
                                        Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])]
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
    PlacementEffect[Measured[Rect[Float], Bounds, DesktopPlacedWidget[Event]]],
    Rect[Float],
    Bounds,
    List,
    Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])]
  ]
) : DesktopWidget[Event]  =
  containerWidget[List, Event](
    children,
    placementStrategy
  )
end listContainerWidget
