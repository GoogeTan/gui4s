package gui4s.desktop.kit
package widgets

import catnip.syntax.all.{_, given}
import cats._
import cats.effect._

import gui4s.core.geometry._
import gui4s.core.layout.rowcolumn.{PlacementStrategy => GenericPlacementStrategy}

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopWidget

def stackContainer[IO[_] : Sync as S, Event](
                                              children : List[DesktopWidget[IO, Event]],
                                              verticalPlacement : OneElementLinearContainerPlacementStrategy[IO],
                                              horizontalPlacement : OneElementLinearContainerPlacementStrategy[IO]
) : DesktopWidget[IO, Event] =
  given Ordering[Point2d[Float]] = Ordering.by(point => math.max(point.x, point.y))
  gui4s.core.widget.library.stackContainer[
    DesktopPlacedWidget[IO, Event],
    OuterPlaceC[IO],
    Bounds,
    Float,
  ](
    getBounds = OuterPlace.getBounds[IO],
    container = containerWidget[IO, List, Event](traverseOrdered)
  )(
    children,
    GenericPlacementStrategy.PlaceIndependently(
      GenericPlacementStrategy.Zip(Axis.Vertical, verticalPlacement, horizontalPlacement),
      Point2d(0f, 0f)
    )
  )
end stackContainer
