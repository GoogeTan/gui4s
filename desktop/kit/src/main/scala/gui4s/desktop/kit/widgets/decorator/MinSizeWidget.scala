package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.{*, given}
import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.{Point2d, Point3d, Rect}
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.{ElementPlacementResult, OneElementPlacementStrategy, PlacementStrategy}
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.kit.widgets.*

def minSizeWidget[Event](
                          minSize : Rect[Float],
                          placeIfSmaller : OneElementPlacementStrategy[PlacementEffectC[IO], Rect[Float], Bounds, Point2d[Float]],
                        )
  : Decorator[DesktopWidget[Event]] =
  gui4s.core.widget.library.decorator.minSizeWidget[
    DesktopPlacedWidget[Event],
    PlacementEffectC[IO],
    Situated,
    Rect[Float],
    Point3d[Float]
  ](
    oneElementContainerWidget,
    {
      case (Sized(widget, size), minSize) =>
        if size.width < minSize.width || size.height < minSize.height then
          PlacementEffect.getBounds[IO]
            .flatMap(placeIfSmaller(size, _))
            .map { case ElementPlacementResult(_, coordinatesOfStarts) =>
              Sized(
                (widget, new Point3d(coordinatesOfStarts, 0)),
                minSize
              )
          }
        else
          Sized((widget, Point3d.Zero[Float]), size).pure[PlacementEffectC[IO]]
        end if
    }
  )(minSize)
end minSizeWidget

def minSizeWidget[Event](
                          minSize : Rect[Float],
                          placeHorizontally : OneElementLinearContainerPlacementStrategy,
                          placeVertically : OneElementLinearContainerPlacementStrategy,
                        )
: Decorator[DesktopWidget[Event]] =
    minSizeWidget(minSize, PlacementStrategy.Zip(placeHorizontally, placeVertically))
end minSizeWidget
