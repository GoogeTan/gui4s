package gui4s.desktop.kit.cats
package widgets

import effects.*
import effects.Place.given
import effects.Update.given

import cats.Monad
import cats.effect.IO
import gui4s.desktop.widget.library.decorator.{MapEvent, mapUpdate as genericMapUpdate}

def mapEventWidget : MapEvent[DesktopWidget] =
  gui4s.desktop.kit.widgets.mapEventWidget[IO]
end mapEventWidget

extension[IO[_] : Monad, Event](value : DesktopWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
