package gui4s.android.kit.widgets

import gui4s.core.kit.effects.RecompositionReaction
import gui4s.android.kit.effects.*
import gui4s.desktop.widget.library.Widget

type AndroidPlacedWidget[IO[_], Event] = Widget[UpdateC[IO, Event], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent]
type AndroidWidget[IO[_], Event] = Place[IO, AndroidPlacedWidget[IO, Event]]
