package gui4s.desktop.kit.cats
package widgets

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import glfw4s.core.types.GlfwError
import gui4s.desktop.widget.library.decorator.MapEvent

def mapEventWidget : MapEvent[DesktopWidget] =
  gui4s.desktop.kit.common.widgets.mapEventWidget[EitherT[IO, GlfwError, *]]
end mapEventWidget

extension[Event](value : DesktopWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
