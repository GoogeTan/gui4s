package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import widgets.*

import cats.effect.IO

import cats.effect.std.Supervisor
import gui4s.desktop.widget.library.LaunchedEffectWidget

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEvent[
  Event : Typeable,
  Key : Typeable
](
  supervisor: Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
  eventFromAny : Any => Option[Event]
) : LaunchedEffectWidget[DesktopWidget[Event], Key, IO[Event]] =
  gui4s.desktop.kit.widgets.decorator.launchedEvent(supervisor, raiseExternalEvent, eventFromAny)
end launchedEvent
