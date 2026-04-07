package gui4s.android.kit.widgets

import catnip.syntax.all.given
import cats.effect.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.drawAt
import gui4s.android.kit.effects.Place.given
import gui4s.core.geometry.{Axis, InfinityOr, Point3d, Rect}
import gui4s.core.layout.{Measured, Sized, Weighted}
import gui4s.core.widget.library.{WeightedLinearContainer, measureIncrementally}
import gui4s.desktop.widget.library.*

def weightedLinearContainer[
  Event
] : WeightedLinearContainer[
  AndroidWidget[Event],
  PlacementEffect,
  List,
  InfinityOr[Float],
  Float,
  Axis
] =
  type WidgetWithMeta = Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Option[Float], Point3d[Float])]
  gui4s.core.widget.library.weightedLinearContainer(
    (children, placement) =>
      gui4s.desktop.widget.library.container[
        UpdateC[Event],
        Place,
        List,
        Draw,
        RecompositionReaction,
        Weighted[AndroidWidget[Event]],
        WidgetWithMeta
      ](
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
          case (Measured((widget, weight, position), _, _), path) =>
            Update.withCornerCoordinates(widgetHandlesEvent(widget, path), _ + position)
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
          yield placed
            .mapValue(
              _.map {
                case (widget, Measured((position, weight), size, bounds)) =>
                  Measured((widget, position, weight), size, bounds)
              }
            ),
        incrementalFreeChildrenFromPlaced =
          (oldWidget, newWidget) =>
            newWidget.getOrElse(
              Weighted(measureIncrementally[
                PlacementEffect,
                AndroidPlacedWidget[Event],
                Rect[Float],
                Bounds,
                Point3d[Float]
              ](PlacementEffect.getBounds, widgetAsFree, oldWidget.map(_._1)), oldWidget.value._2)
            ),
      ),
    PlacementEffect.getBounds,
    (rect, effect) => PlacementEffect.withBounds(effect, _ => rect),
    _.minus(_),
    (bounds, weight) => bounds * weight,
  )
end weightedLinearContainer
