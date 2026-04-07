package gui4s.android.kit.widgets.decorator

import catnip.syntax.all.given
import cats.*
import cats.effect.IO
import cats.syntax.all.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.PlacementEffect.given
import gui4s.android.kit.widgets.*
import gui4s.core.geometry.{InfinityOr, Point2d, Point3d, Rect}
import gui4s.core.layout.*
import gui4s.core.widget.library.decorator.Decorator

def minSizeWidget[Event](
  minSize : Rect[Float],
  placeIfSmaller : OneElementPlacementStrategy[PlacementEffect, Rect[Float], Rect[Float], Bounds, Point2d[Float]],
) : Decorator[AndroidWidget[Event]] =
  type Res = Measured[Rect[Float], Bounds, (AndroidPlacedWidget[Event], Point3d[Float])]
  type Free = PlacementEffect[Measured[Rect[Float], Bounds, AndroidPlacedWidget[Event]]]
  gui4s.core.widget.library.decorator.minSizeWidget[
    AndroidWidget[Event],
    AndroidPlacedWidget[Event],
    Measured[Rect[Float], Bounds, AndroidPlacedWidget[Event]],
    PlacementEffect,
    Rect[Float],
    Bounds,
    Point3d[Float],
    Res,
  ](
    containerWidget = oneElementContainerWidget,
    getBounds = PlacementEffect.getBounds,
    ensureMinimalSize = (childSize, bounds) =>
      if childSize.width < minSize.width || childSize.height < minSize.height then
        placeIfSmaller(childSize, bounds)
          .map { case Sized(coordinatesOfStarts, _) =>
            Sized(
              new Point3d(coordinatesOfStarts, 0.0),
              minSize,
            )
          }
      else
        Sized[Rect[Float], Point3d[Float]](Point3d.Zero[Float], minSize).pure[PlacementEffect],
    makeMeta = (sizedWidget, bounds, point) => Measured((sizedWidget.value, point), sizedWidget.size, bounds),
    itemSize = _.size
  )(minSize.map(new InfinityOr(_)))
end minSizeWidget

def minSizeWidget[Event](
                          minSize : Rect[Float],
                          placeHorizontally : OneElementLinearContainerPlacementStrategy,
                          placeVertically : OneElementLinearContainerPlacementStrategy,
                        )
: Decorator[AndroidWidget[Event]] =
    minSizeWidget[Event](minSize, PlacementStrategy.Zip[PlacementEffect, Float, InfinityOr[Float], Id, Float](placeHorizontally, placeVertically))
end minSizeWidget
