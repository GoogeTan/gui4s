package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.Place.given
import effects.Update.given

import gui4s.decktop.widget.library.decorator.{MapEvent, mapUpdate as genericMapUpdate}

def mapEventWidget : MapEvent[DesktopWidget] =
  [Event, NewEvent] => f => original =>
    genericMapUpdate[UpdateC[Event], UpdateC[NewEvent], Place, Draw, RecompositionReaction, DownEvent](
      original,
      Update.mapEvents(_.map(f))
    )
end mapEventWidget


extension[Event](value : DesktopWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
  
