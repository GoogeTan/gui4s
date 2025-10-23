package gui4s.desktop.kit.zio
package widgets

import cats.Monad
import gui4s.desktop.widget.library.decorator.MapEvent
import zio.*
import zio.interop.catz.*

def mapEventWidget : MapEvent[DesktopWidget] =
  gui4s.desktop.kit.common.widgets.mapEventWidget[Task]
end mapEventWidget


extension[IO[_] : Monad, Event](value : DesktopWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
