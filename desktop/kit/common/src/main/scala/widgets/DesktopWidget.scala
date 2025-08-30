package gui4s.desktop.kit
package widgets

import effects.*
import gui4s.decktop.widget.library.Widget

type DesktopPlacedWidget[IO[_], Event] = Widget[UpdateC[IO, Event], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent]
type DesktopWidget[IO[_], Event] = Place[IO, DesktopPlacedWidget[IO, Event]]
