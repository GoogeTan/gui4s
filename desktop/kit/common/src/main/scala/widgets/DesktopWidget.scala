package gui4s.desktop.kit
package widgets

import effects.*
import gui4s.desktop.widget.library.Widget
import gui4s.core.kit.effects.RecompositionReaction 

type DesktopPlacedWidget[IO[_], Event] = Widget[UpdateC[IO, Event], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent]
type DesktopWidget[IO[_], Event] = Place[IO, DesktopPlacedWidget[IO, Event]]
