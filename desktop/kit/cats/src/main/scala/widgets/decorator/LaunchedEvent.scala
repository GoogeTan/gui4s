package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*

import cats.effect.IO
import cats.effect.std.Supervisor
import gui4s.decktop.widget.library.{launchedEvent as genericLaunchedEvent, LaunchedEffectWidget}
import gui4s.core.widget.Path
import scala.reflect.Typeable
import cats.syntax.all.* 

def launchedEvent[Event : Typeable, Key : Typeable](supervisor: Supervisor[IO], raiseExternalEvent : DownEvent => IO[Unit]) : LaunchedEffectWidget[DesktopWidget[Event], Key, IO[Event]] =
  genericLaunchedEvent[
    IO,
    DesktopWidget[Event],
    Key,
    Update,
    InnerPlace[DesktopPlacedWidget[Event]],
    DownEvent,
    Event
  ](
    launchedEffectWidget = launchedEffect(supervisor),
    eventCatcher = eventCatcher,
    pushEvent = (path, event) => raiseExternalEvent(DownEvent.ExternalEventForWidget(path, event)),
    catchEvent = (path, event) =>
      DownEvent.catchExternalEvent(path, event) match
        case None =>
          false.pure[UpdateC[Event]]
        case Some[Any](event : Event) =>
          Update.emitEvents(List(event)).as(true)
        case Some[Any](valueFound : Any) =>
          Update.raiseError("Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString)
  )
end launchedEvent
