package gui4s.android.kit.widgets.decorator

import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.{AndroidPlacedWidget, AndroidWidget}
import gui4s.core.widget.library.{LaunchedEffectWidget, launchedEvent as genericLaunchedEvent}

import scala.reflect.Typeable

import cats.effect.IO
import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.{AndroidPlacedWidget, AndroidWidget}
import gui4s.core.widget.library.{LaunchedEffectWidget, launchedEvent as genericLaunchedEvent}

import scala.reflect.Typeable

trait LaunchedEvent:
  def apply[Key : Typeable, Event](
                                     name : String,
                                     key: Key,
                                     task : IO[Event],
                                     body : AndroidWidget[Event]
                                   ) : AndroidWidget[Event]
end LaunchedEvent

object LaunchedEvent:
  def apply[
    Event,
    Key : Typeable
  ](
    supervisor: Supervisor[IO],
    raiseExternalEvent : DownEvent => IO[Unit],
    eventFromAny : Any => Option[Event]
  ) : LaunchedEffectWidget[AndroidWidget[Event], Key, IO[Event]] =
    genericLaunchedEvent[
      IO,
      AndroidWidget[Event],
      Key,
      Update[*, *],
      Situated[AndroidPlacedWidget[Event]],
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
          case Some[Any](valueFound : Any) =>
            eventFromAny(valueFound).fold(
              Update.raiseError(new Exception("Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString))
            )(
              event => Update.emitEvents(List(event)).as(true)
            )
    )
  end apply
end LaunchedEvent

