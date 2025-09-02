package gui4s.desktop.kit
package common.widgets

import common.effects.*
import common.effects.Place.given

import cats.Monad
import gui4s.desktop.widget.library.decorator.{MapEvent, mapUpdate as genericMapUpdate}

def mapEventWidget[IO[_] : Monad] : MapEvent[DesktopWidget[IO, *]] =
  [Event, NewEvent] => f => original =>
    genericMapUpdate[UpdateC[IO, Event], UpdateC[IO, NewEvent], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent](
      original,
      Update.mapEvents[IO, Event, NewEvent](f)
    )
end mapEventWidget


extension[IO[_] : Monad, Event](value : DesktopWidget[IO, Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[IO, NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
