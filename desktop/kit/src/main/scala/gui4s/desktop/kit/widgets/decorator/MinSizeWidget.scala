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
  placeIfSmaller : OneElementPlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Bounds, Point2d[Float]],
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
      if childSize.width < minSize.width || childSize.height < minSize.height then
        PlacementEffect.getBounds
          .flatMap(placeIfSmaller(childSize, _))
          .map { case Sized(coordinatesOfStarts, _) =>
            Sized(
              new Point3d(coordinatesOfStarts, 0.0),
              minSize,
            )
          }
      else
        Sized(Point3d.Zero[Float], minSize).pure[PlacementEffect],
    makeMeta = (sizedWidget, bounds, point) => Measured((sizedWidget.value, point), sizedWidget.size, bounds),
    itemSize = _.size
  )(minSize.map(new InfinityOr(_)))
end minSizeWidget

def minSizeWidget[Event](
                          minSize : Rect[Float],
                          placeHorizontally : OneElementLinearContainerPlacementStrategy,
                          placeVertically : OneElementLinearContainerPlacementStrategy,
                        )
: Decorator[DesktopWidget[Event]] =
    minSizeWidget(minSize, PlacementStrategy.Zip(placeHorizontally, placeVertically))
end minSizeWidget
