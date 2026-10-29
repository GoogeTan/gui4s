package gui4s.android.kit.widgets

import cats.syntax.all.*

import gui4s.core.geometry.Axis
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized

import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.decorator.clipWidget

def lightScrollWidget[Event](
                               body : AndroidWidget[Event],
                               direction : Axis,
                               shiftProvider : (Point3d[Float] => AndroidWidget[Event]) => AndroidWidget[Event],
                               boundsForClip : ShapeInBounds
                             ) : AndroidWidget[Event] =
  gui4s.core.widget.library.scrollWidget[
    AndroidWidget[Event],
    AndroidPlacedWidget[Event],
    PlacementEffect,
    Rect[Float],
    Bounds,
    Point3d[Float],
    Point3d[Float]
  ](
    containerWidget = oneElementContainerWidget,
    scrolledShiftAndClip = callback =>
      clipWidget(
        shiftProvider(callback),
        boundsForClip
      ),
    withInfiniteBounds = PlacementEffect.withBoundsK(_.mapAlong(direction, _ => InfinityOr(None))),
    body = body,
    placementStrategy = (childSize, bounds) =>
      Sized(
        value = Point3d(0f, 0f, 0f),
        size = Rect(
          bounds.width.value.fold(childSize.width)(childSize.width.min(_)),
          bounds.height.value.fold(childSize.height)(childSize.height.min(_)),
        )
      ).pure,
    combinePoints = _.add(_)
  )
end lightScrollWidget
