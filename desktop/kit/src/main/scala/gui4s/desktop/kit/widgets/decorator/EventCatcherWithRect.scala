package gui4s.desktop.kit
package widgets.decorator

import cats.effect.IO

import gui4s.core.widget.library.decorator.EventCatcherWithRect

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.widget.library._
import gui4s.desktop.widget.library.decorator.eventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  PlacementEffect[Situated[DesktopPlacedWidget[Event]]],
  Update[Event, Boolean],
  Situated[DesktopPlacedWidget[Event]],
  DownEvent
] = eventCatcherWithRect[
  DesktopPlacedWidget[Event],
  UpdateC[Event],
  PlacementEffect,
  Situated,
  DownEvent
](
  updateDecorator[Event],
  Update.markEventHandled[Event],
  widgetAsFree[UpdateC[Event], Place, Draw, RecompositionReaction, DownEvent],
  widgetHandlesEvent[UpdateC[Event], Place, Draw, RecompositionReaction, DownEvent]
)
