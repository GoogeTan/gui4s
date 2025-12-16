package gui4s.desktop.kit
package widgets

import cats.*
import cats.effect.*
import catnip.syntax.all.{*, given}
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.PlacementStrategy as GenericPlacementStrategy
import gui4s.core.widget.library.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
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
    container = container[IO, List, Event](traverseOrdered)
  )(
    children,
    GenericPlacementStrategy.PlaceIndependently(
      GenericPlacementStrategy.Zip(Axis.Vertical, verticalPlacement, horizontalPlacement),
      Point2d(0f, 0f)
    )
  )
end stackContainer
