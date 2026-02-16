package gui4s.desktop.kit
package widgets

import cats.effect.*

import gui4s.core.widget.library.MapEvent

import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.widget.library.decorator.{mapUpdate => genericMapUpdate}

def mapEventWidget : MapEvent[DesktopWidget] =
  [Event, NewEvent] => f => original =>
    genericMapUpdate[UpdateC[IO, Event], UpdateC[IO, NewEvent], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent](
      original,
      Update.mapEvents[IO, Event, NewEvent](f)
    )
end mapEventWidget


extension[Event](value : DesktopWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
