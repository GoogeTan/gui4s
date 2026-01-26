package gui4s.desktop.kit.widgets.decorator

import catnip.syntax.all.{_, given}
import cats._
import cats.effect._
import cats.syntax.all._

import gui4s.core.geometry.Axis
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Point3d
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.ElementPlacementResult
import gui4s.core.layout.rowcolumn.OneElementPlacementStrategy
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.kit.effects.PlacementEffect.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets._

def minSizeWidget[IO[_] : Sync, Event](
                                       minSize : Rect[Float],
                                       placeIfSmaller : OneElementPlacementStrategy[PlacementEffectC[IO], Rect[Float], Bounds, Point2d[Float]],
                                      )
  : Decorator[DesktopWidget[IO, Event]] =
  gui4s.core.widget.library.decorator.minSizeWidget[
    DesktopPlacedWidget[IO, Event],
    PlacementEffectC[IO],
    Situated,
    Rect[Float],
    Point3d[Float]
  ](
    containerWidget(traverseOne),
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

def minSizeWidget[IO[_] : Sync, Event](
                                        minSize : Rect[Float],
                                        placeHorizontally : OneElementLinearContainerPlacementStrategy[IO],
                                        placeVertically : OneElementLinearContainerPlacementStrategy[IO],
                                      )
: Decorator[DesktopWidget[IO, Event]] =
    minSizeWidget(minSize, PlacementStrategy.Zip(Axis.Horizontal, placeHorizontally, placeVertically))
end minSizeWidget
