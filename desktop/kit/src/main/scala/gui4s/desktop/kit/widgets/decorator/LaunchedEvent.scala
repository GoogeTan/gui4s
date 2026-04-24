package gui4s.desktop.kit
package widgets.decorator

import scala.reflect.Typeable

import cats.*
import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*

import gui4s.core.widget.library.LaunchedEffectWidget
import gui4s.core.widget.library.launchedEvent as genericLaunchedEvent

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopPlacedWidget
import gui4s.desktop.kit.widgets.DesktopWidget

trait LaunchedEvent:
  def apply[Key : Typeable, Event](
                                     name : String,
                                     key: Key,
                                     task : IO[Event],
                                     body : DesktopWidget[Event]
                                   ) : DesktopWidget[Event]
end LaunchedEvent

object LaunchedEvent:
  def apply[
    Event,
    Key : Typeable
  ](
    supervisor: Supervisor[IO],
    raiseExternalEvent : DownEvent => IO[Unit],
    eventFromAny : Any => Option[Event]
  ) : LaunchedEffectWidget[DesktopWidget[Event], Key, IO[Event]] =
    genericLaunchedEvent[
      IO,
      DesktopWidget[Event],
      Key,
      Update,
      Situated[DesktopPlacedWidget[Event]],
      DownEvent,
      Event
    ](
      launchedEffectWidget = launchedEffect(supervisor),
      eventCatcher = eventCatcher,
      pushEvent = (path, event) => raiseExternalEvent(DownEvent.ExternalEventForWidget(path, event)),
      catchEvent =
        Update.handleExternalEvents_(
          valueFound =>
            eventFromAny(valueFound).fold(
               Update.raiseError[Event, Boolean](path => new Exception("Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString))
            )(
              event => Update.emitEvents[Event](List(event)).as(true)
            )
        )
    )
  end apply
end LaunchedEvent

