package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import widgets.*

import cats.data.EitherT
import cats.effect.IO
import cats.effect.std.Supervisor
import glfw4s.core.types.GlfwError
import gui4s.desktop.widget.library.LaunchedEffectWidget

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEvent[
  Event : Typeable,
  Key : Typeable
](
  supervisor: Supervisor[EitherT[IO, GlfwError, *]],
  raiseExternalEvent : DownEvent => EitherT[IO, GlfwError, Unit],
  eventFromAny : Any => Option[Event]
) : LaunchedEffectWidget[DesktopWidget[Event], Key, EitherT[IO, GlfwError, Event]] =
  gui4s.desktop.kit.common.widgets.decorator.launchedEvent[
    EitherT[IO, GlfwError, *],
    Event,
    Key
  ](supervisor, raiseExternalEvent, eventFromAny)
end launchedEvent
