package gui4s.desktop.kit
package widgets.decorator

import effects.*
import widgets.*

import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import gui4s.desktop.widget.library.{LaunchedEffectWidget, launchedEvent as genericLaunchedEvent}

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEvent[
  IO[_] : MonadThrow,
  Event,
  Key : Typeable
](
  supervisor: Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
  eventFromAny : Any => Option[Event]
) : LaunchedEffectWidget[DesktopWidget[IO, Event], Key, IO[Event]] =
  genericLaunchedEvent[
    IO,
    DesktopWidget[IO, Event],
    Key,
    Update[IO, *, *],
    InnerPlace[DesktopPlacedWidget[IO, Event]],
    DownEvent,
    Event
  ](
    launchedEffectWidget = launchedEffect(supervisor),
    eventCatcher = eventCatcher,
    pushEvent = (path, event) => raiseExternalEvent(DownEvent.ExternalEventForWidget(path, event)),
    catchEvent = (path, event) =>
      DownEvent.catchExternalEvent(path, event) match
        case None =>
          false.pure[UpdateC[IO, Event]]
        case Some[Any](valueFound : Any) =>
          eventFromAny(valueFound).fold(
            Update.raiseError(new Exception("Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString))
          )(
            event => Update.emitEvents(List(event)).as(true)
          )
  )
end launchedEvent
