package gui4s.desktop.kit
package widgets

import cats.*
import catnip.syntax.all.given
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.PlacementStrategy as GenericPlacementStrategy
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
import gui4s.desktop.kit.widgets.DesktopWidget

def stackContainer[IO[_] : Monad, Event](
  children : List[DesktopWidget[IO, Event]],
  verticalPlacement : OneElementPlacementStrategy[IO],
  horizontalPlacement : OneElementPlacementStrategy[IO]
) : DesktopWidget[IO, Event] =
  given Ordering[Point2d[Float]] = Ordering.by(point => math.max(point.x, point.y))
  gui4s.desktop.widget.library.stackContainer[
    UpdateC[IO, Event],
    OuterPlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    Bounds,
    Float
  ](
    Update.isEventHandled,
    OuterPlace.getBounds,
  )(
    children,
    GenericPlacementStrategy.PlaceIndependently(
      GenericPlacementStrategy.Zip(Axis.Vertical, verticalPlacement, horizontalPlacement),
      Point2d(0f, 0f)
    )
  )
end stackContainer
