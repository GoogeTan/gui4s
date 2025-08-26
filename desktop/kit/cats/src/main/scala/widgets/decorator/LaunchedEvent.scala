package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*

import cats.effect.IO
import cats.effect.std.Supervisor
import gui4s.decktop.widget.library.{launchedEvent as genericLaunchedEvent, LaunchedEffectWidget}
import io.github.humbleui.skija.Path
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
    pushEvent = (path, event) => raiseExternalEvent(SkijaDownEvent.ExternalEventForWidget(path, event)),
    catchEvent = (path, event) =>
      DownEvent.catchExternalEvent(path, event, (valueFound : Any) => "Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString) match
        case None => false.pure[UpdateC[Event]]
        case Some(Right(event)) =>
          Update.emitEvents(List(event)).as(true)
        case Some(Left(error)) =>
          Update.raiseError(error)
  )
end launchedEvent
