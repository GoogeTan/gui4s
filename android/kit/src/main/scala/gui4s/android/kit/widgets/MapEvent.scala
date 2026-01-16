package gui4s.android.kit.widgets

import gui4s.core.widget.library.MapEvent
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given
import gui4s.desktop.widget.library.decorator.mapUpdate as genericMapUpdate

def mapEventWidget[IO[_] : Monad] : MapEvent[AndroidWidget[IO, *]] =
  [Event, NewEvent] => f => original =>
    genericMapUpdate[UpdateC[IO, Event], UpdateC[IO, NewEvent], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent](
      original,
      Update.mapEvents[IO, Event, NewEvent](f)
    )
end mapEventWidget


extension[IO[_] : Monad, Event](value : AndroidWidget[IO, Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : AndroidWidget[IO, NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
