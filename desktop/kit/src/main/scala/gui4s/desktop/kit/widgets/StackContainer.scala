package gui4s.desktop.kit
package widgets

import cats.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
import gui4s.desktop.kit.widgets.DesktopWidget

def stackContainer[IO[_] : Monad, Event](
  children : List[DesktopWidget[IO, Event]],
  xyPlacement : PlacementStrategy[IO, Bounds, List, Point2d[Float]]
) : DesktopWidget[IO, Event] =
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
  )(children, xyPlacement)
end stackContainer
