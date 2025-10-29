package gui4s.desktop.kit
package widgets.decorator

import effects.*
import effects.Place.given
import widgets.DesktopPlacedWidget

import cats.Monad
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.{EventCatcherWithRect, eventCatcherWithRect}

def eventCatcher[IO[_] : Monad, Event]: EventCatcherWithRect[
  OuterPlace[IO, InnerPlace[DesktopPlacedWidget[IO, Event]]],
  Update[IO, Event, Boolean],
  InnerPlace[DesktopPlacedWidget[IO, Event]],
  DownEvent
] = eventCatcherWithRect[
  DesktopPlacedWidget[IO, Event],
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
