package gui4s.android.kit.widgets.decorator

import gui4s.core.widget.library.decorator.EventCatcherWithRect
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.eventCatcherWithRect

def eventCatcher[IO[_] : Monad, Event]: EventCatcherWithRect[
  OuterPlace[IO, InnerPlace[AndroidPlacedWidget[IO, Event]]],
  Update[IO, Event, Boolean],
  InnerPlace[AndroidPlacedWidget[IO, Event]],
  DownEvent
] = eventCatcherWithRect[
  AndroidPlacedWidget[IO, Event],
  UpdateC[IO, Event],
  OuterPlace[IO, *],
  InnerPlace,
  DownEvent
](
  updateDecorator[IO, Event],
  Update.markEventHandled[IO, Event],
  widgetAsFree[UpdateC[IO, Event], Place[IO, *], Draw[IO], RecompositionReaction[IO], DownEvent],
  widgetHandlesEvent[UpdateC[IO, Event], Place[IO, *], Draw[IO], RecompositionReaction[IO], DownEvent]
)
