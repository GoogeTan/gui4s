package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*
import gui4s.decktop.widget.library.*
import gui4s.decktop.widget.library.decorator.{eventCatcherWithRect, EventCatcherWithRect}

def eventCatcher[Event]: EventCatcherWithRect[
  OuterPlace[InnerPlace[DesktopPlacedWidget[Event]]],
  UpdateC[Event][Boolean],
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] = eventCatcherWithRect[
  DesktopPlacedWidget[Event],
  UpdateC[Event],
  OuterPlace,
  InnerPlace,
  DownEvent
](
  updateDecorator[Event],
  Update.markEventHandled[List[Event]],
  widgetAsFree[UpdateC[Event], Place, Draw, RecompositionReaction, DownEvent],
  widgetHandlesEvent[UpdateC[Event], Place, Draw, RecompositionReaction, DownEvent]
)
