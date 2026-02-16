package gui4s.desktop.kit
package widgets.decorator

import cats.effect.IO

import gui4s.core.widget.library.decorator.EventCatcherWithRect

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library._
import gui4s.desktop.widget.library.decorator.eventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  PlacementEffect[IO, Situated[DesktopPlacedWidget[Event]]],
  Update[IO, Event, Boolean],
  Situated[DesktopPlacedWidget[Event]],
  DownEvent
] = eventCatcherWithRect[
  DesktopPlacedWidget[Event],
  UpdateC[IO, Event],
  PlacementEffect[IO, *],
  Situated,
  DownEvent
](
  updateDecorator[Event],
  Update.markEventHandled[IO, Event],
  widgetAsFree[UpdateC[IO, Event], Place[IO, *], Draw[IO], RecompositionReaction[IO], DownEvent],
  widgetHandlesEvent[UpdateC[IO, Event], Place[IO, *], Draw[IO], RecompositionReaction[IO], DownEvent]
)
