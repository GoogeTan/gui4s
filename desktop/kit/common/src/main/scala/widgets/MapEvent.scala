package gui4s.desktop.kit
package widgets

import effects.*
import effects.Place.given
import effects.Update.given

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
