package gui4s.desktop.kit
package widgets.decorator

import cats.effect.IO

import gui4s.core.widget.library.decorator.EventCatcherWithRect

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.widget.library._
import gui4s.desktop.widget.library.decorator.eventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  DesktopWidget[Event],
  Update[Event, Unit],
  Situated[DesktopPlacedWidget[Event]]
] = eventCatcherWithRect[
  DesktopPlacedWidget[Event],
  UpdateC[Event],
  PlacementEffect,
  Situated
](
  updateDecorator[Event],
  widgetAsFree[UpdateC[Event], Place, Draw, RecompositionReaction],
  widgetHandlesEvent[UpdateC[Event], Place, Draw, RecompositionReaction]
)
