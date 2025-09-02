package gui4s.desktop.kit
package common.widgets

import gui4s.core.kit.effects.RecompositionReaction
import gui4s.desktop.kit.common.effects.{Place, PlaceC, UpdateC, Draw, DownEvent}
import gui4s.desktop.widget.library.Widget

type DesktopPlacedWidget[IO[_], Event] = Widget[UpdateC[IO, Event], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent]
type DesktopWidget[IO[_], Event] = Place[IO, DesktopPlacedWidget[IO, Event]]
