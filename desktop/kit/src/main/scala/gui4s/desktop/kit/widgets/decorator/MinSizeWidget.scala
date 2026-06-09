package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.given
import cats.*
import cats.effect.*
import cats.syntax.all.*

import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.Measured
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy
import gui4s.core.layout.Sized
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.kit.widgets.*

def minSizeWidget[Event](
  minSize : Rect[Float],
  placeHorizontallyIfSmaller : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float],
  placeVerticallyIfSmaller : OneElementPlacementStrategy[PlacementEffect, Float, Float, Float, Float],
) : Decorator[DesktopWidget[Event]] =
  gui4s.core.widget.library.decorator.minSizeWidget[
    DesktopWidget[Event],
    DesktopPlacedWidget[Event],
    Measured[Rect[Float], Bounds, DesktopPlacedWidget[Event]],
    PlacementEffect,
    Rect[Float],
    Bounds,
    Point3d[Float],
    Measured[Rect[Float], Bounds, (DesktopPlacedWidget[Event], Point3d[Float])],
  ](
    oneElementContainerWidget,
    PlacementEffect.getBounds,
    (childSize, bounds) =>
      for
        horizontally <- if childSize.width < minSize.width then
          placeHorizontallyIfSmaller(childSize.width, minSize.width).map {
            case Sized(place, size) => Sized(place, minSize.width)
          }
        else
          Sized(0f, childSize.width).pure[PlacementEffect]
        vertically <- if childSize.height < minSize.height then
          placeVerticallyIfSmaller(childSize.height, minSize.height).map {
            case Sized(place, size) => Sized(place, minSize.height)
          }
        else
          Sized(0f, childSize.height).pure[PlacementEffect]
      yield
        Sized(Point3d(horizontally.value, vertically.value, 0f), Rect(horizontally.size, vertically.size)),
    makeMeta = (sizedWidget, bounds, point) => Measured((sizedWidget.value, point), sizedWidget.size, bounds),
    itemSize = _.size
  )(minSize.map(new InfinityOr(_)))
end minSizeWidget
