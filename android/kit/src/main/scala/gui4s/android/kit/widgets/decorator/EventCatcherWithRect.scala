package gui4s.android.kit.widgets.decorator

import gui4s.core.widget.library.decorator.EventCatcherWithRect
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.eventCatcherWithRect

import cats.effect.IO
import cats.Monad
import gui4s.core.widget.library.decorator.EventCatcherWithRect
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidPlacedWidget
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.eventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  PlacementEffect[Situated[AndroidPlacedWidget[Event]]],
  Update[Event, Boolean],
  Situated[AndroidPlacedWidget[Event]],
  DownEvent
] = eventCatcherWithRect[
  AndroidPlacedWidget[Event],
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
