package gui4s.desktop.kit
package widgets

import effects.*
import effects.Draw.given
import effects.RecompositionReaction.given
import widgets.DesktopWidget

import cats.*
import gui4s.core.geometry.*

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
