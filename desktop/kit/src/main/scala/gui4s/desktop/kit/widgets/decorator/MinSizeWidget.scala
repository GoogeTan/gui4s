package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.given
import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.{InfinityOr, Point2d, Point3d, Rect}
import gui4s.core.layout.{ElementPlacementResult, Measured, OneElementPlacementStrategy, PlacementStrategy}
import gui4s.core.widget.library.LayersMetadata
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
    Measured[Float, InfinityOr[Float], DesktopPlacedWidget[Event]],
    PlacementEffect,
    Rect[Float],
    Bounds,
    Point3d[Float],
    LayersMetadata[Point3d[Float], Rect[Float], Bounds],
  ](
    oneElementContainerWidget,
    PlacementEffect.getBounds,
    (childSize, bounds) =>
      if childSize.width < minSize.width || childSize.height < minSize.height then
        PlacementEffect.getBounds
          .flatMap(placeIfSmaller(childSize, _))
          .map { case ElementPlacementResult(_, coordinatesOfStarts) =>
            ElementPlacementResult(
              minSize,
              new Point3d(coordinatesOfStarts, 0),
            )
          }
      else
        ElementPlacementResult[Id, Rect[Float], Point3d[Float]](minSize, Point3d.Zero[Float]).pure[PlacementEffect],
    makeMeta = (sizedWidget, bounds, point) => (sizedWidget.value, LayersMetadata(point, sizedWidget.size, bounds)),
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
