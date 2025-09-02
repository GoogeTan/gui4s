package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import widgets.*
import cats.effect.std.Supervisor
import gui4s.desktop.widget.library.LaunchedEffectWidget
import zio.*
import zio.interop.catz.*

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEvent[
  Event : Typeable,
  Key : Typeable
](
  supervisor: Supervisor[Task],
  raiseExternalEvent : DownEvent => Task[Unit],
  eventFromAny : Any => Option[Event]
) : LaunchedEffectWidget[DesktopWidget[Event], Key, Task[Event]] =
  gui4s.desktop.kit.widgets.decorator.launchedEvent(supervisor, raiseExternalEvent, eventFromAny)
end launchedEvent
