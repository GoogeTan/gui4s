package gui4s.android.kit.widgets.decorator

import gui4s.core.widget.library.decorator.EventCatcherWithRect
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
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
  AndroidWidget[Event],
  Update[Event, Unit],
  Situated[AndroidPlacedWidget[Event]]
] = eventCatcherWithRect[
  AndroidPlacedWidget[Event],
  UpdateC[Event],
  PlacementEffect,
  Situated
](
  updateDecorator[Event],
  widgetAsFree[UpdateC[Event], Place, Draw, RecompositionReaction],
  widgetHandlesEvent[UpdateC[Event], Place, Draw, RecompositionReaction]
)
