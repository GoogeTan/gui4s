package gui4s.desktop.kit.common
package widgets

import cats.*
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.*
import effects.*
import effects.Draw.given
import effects.RecompositionReaction.given
import widgets.DesktopWidget

def stackContainer[IO[_] : Monad, Event](
  children : List[DesktopWidget[IO, Event]],
  xyPlacement : PlacementStrategy[OuterPlaceT[IO], Bounds, List, Point2d[Float]]
) : DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.stackContainer[
    UpdateC[IO, Event],
    OuterPlaceT[IO],
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
