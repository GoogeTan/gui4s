package gui4s.desktop.kit.widgets.decorator

import cats.*
import cats.syntax.all.*
import cats.effect.*
import catnip.syntax.all.{*, given}
import gui4s.core.geometry.{Axis, Point2d, Point3d, Rect}
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.{ElementPlacementResult, OneElementPlacementStrategy, PlacementStrategy}
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.OuterPlace.given
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.DesktopWidget

def minSizeWidget[IO[_] : Sync, Event](
                                       minSize : Rect[Float],
                                       placeIfSmaller : OneElementPlacementStrategy[OuterPlaceC[IO], Bounds, Point2d[Float]],
                                      )
  : Decorator[DesktopWidget[IO, Event]] =
  gui4s.core.widget.library.decorator.minSizeWidget[
    DesktopPlacedWidget[IO, Event],
    OuterPlaceC[IO],
    InnerPlace,
    Rect[Float],
    Point3d[Float]
  ](
    container(traverseOne),
    {
      case (Sized(widget, size), minSize) =>
        if size.width < minSize.width || size.height < minSize.height then
          OuterPlace.getBounds[IO]
            .flatMap(placeIfSmaller(size.toPoint2d, _))
            .map { case ElementPlacementResult(_, coordinatesOfStarts) =>
              Sized(
                (widget, new Point3d(coordinatesOfStarts, 0)),
                minSize
              )
          }
        else
          Sized((widget, Point3d.Zero[Float]), size).pure[OuterPlaceC[IO]]
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
