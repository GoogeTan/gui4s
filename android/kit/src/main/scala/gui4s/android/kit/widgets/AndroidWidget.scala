package gui4s.android.kit.widgets

import cats.effect.IO
import gui4s.core.kit.effects.RecompositionReaction
import gui4s.android.kit.effects.*
import gui4s.desktop.widget.library.Widget

type AndroidPlacedWidget[Event] = Widget[UpdateC[Event], PlaceC, Draw, RecompositionReaction[IO], DownEvent]
type AndroidWidget[Event] = Place[AndroidPlacedWidget[Event]]
