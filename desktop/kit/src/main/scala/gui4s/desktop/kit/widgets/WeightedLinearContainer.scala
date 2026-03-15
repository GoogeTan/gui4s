package gui4s.desktop.kit.widgets

import catnip.syntax.all.given
import catnip.syntax.list.foldOrdered
import catnip.syntax.list.traverseOrdered
import cats.Order
import cats.effect._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.Measured
import gui4s.core.layout.Sized
import gui4s.core.layout.Weighted
import gui4s.core.widget.library._

import gui4s.desktop.kit.effects.Draw.drawAt
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.widget.library._

def weightedLinearContainer[
  Event
] : WeightedLinearContainer[
  DesktopWidget[Event],
  PlacementEffect,
  List,
  InfinityOr[Float],
  Float,
  Axis
] =
  type WidgetWithMeta = Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Option[Float], Point3d[Float])]
  gui4s.core.widget.library.weightedLinearContainer(
    (children, placement) =>
      gui4s.desktop.widget.library.container[
        UpdateC[Event],
        Place,
        List,
        Draw,
        RecompositionReaction,
        DownEvent,
        Weighted[DesktopWidget[Event]],
        WidgetWithMeta
      ](
        isEventConsumed = Update.isEventHandled,
        updateContainerOrdered = children => updateFunction =>
          //Обновляются сначала виджеты, лежащие выше.
          given Order[WidgetWithMeta] = Order.reverse(Order.by(_.value._3.z))
          traverseOrdered[
            UpdateC[Event],
            List,
            WidgetWithMeta
          ](children)(updateFunction),
        drawOrdered = children =>
          // А рисуются вначале виджеты, лежащие ниже.
          given Order[WidgetWithMeta] = Order.by(_.value._3.z)
          foldOrdered[Draw, List](children) {
            case Measured((widget, weight, point), _, _) =>
              drawAt(widgetIsDrawable(widget), point.x, point.y)
          },
        positionedChildHandlesEvent = {
          case (Measured((widget, weight, position), _, _), path, event) =>
            Update.withCornerCoordinates(widgetHandlesEvent(widget, path, event), _ + position)
              .map(
                _.map(
                  Weighted(_, weight)
                )
              )
        },
        positionedMergesWithOldStates = {
          case (Measured((widget, weight, position), _, _), path, states) =>
            widgetMergesWithOldState(widget, path, states)
              .map(
                  Weighted(_, weight)
              )
        },
        positionedReactsOnRecomposition = {
          case (Measured((widget, _, _), _, _), path, states) =>
            widgetReactsOnRecomposition(widget, path, states)
        },
        positionedHasInnerStates = {
          case Measured((widget, _, _), _, _) =>
            widgetHasInnerStates(widget)
        },
        children = children,
        layout = freeChildren =>
          for
            bounds <- PlacementEffect.getBounds
            placed <- placement(freeChildren.map(_.map(_.map(new Measured(_, bounds)))), bounds)
            placedWidgets = placed.coordinates
          yield Sized(
            placedWidgets.map {
              case (widget, Measured((position, weight), size, bounds)) =>
                Measured((widget, position, weight), size, bounds)
            },
            placed.size
          ),
        incrementalFreeChildrenFromPlaced =
          (oldWidget, newWidget) =>
            newWidget.getOrElse(
              Weighted(measureIncrementally[
                PlacementEffect,
                DesktopPlacedWidget[Event],
                Rect[Float],
                Bounds,
                Point3d[Float]
              ](PlacementEffect.getBounds, widgetAsFree, oldWidget.map(_._1)), oldWidget.value._2)
            ),
      ),
    PlacementEffect.getBounds,
    PlacementEffect.setBounds,
    _.minus(_),
    (bounds, weight) => bounds * weight,
  )
end weightedLinearContainer
