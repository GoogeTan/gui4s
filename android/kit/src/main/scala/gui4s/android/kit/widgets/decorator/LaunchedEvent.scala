package gui4s.android.kit.widgets.decorator

import gui4s.core.widget.library.{LaunchedEffectWidget, launchedEvent as genericLaunchedEvent}
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.{AndroidPlacedWidget, AndroidWidget}

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
) : LaunchedEffectWidget[AndroidWidget[IO, Event], Key, IO[Event]] =
  genericLaunchedEvent[
    IO,
    AndroidWidget[IO, Event],
    Key,
    Update[IO, *, *],
    InnerPlace[AndroidPlacedWidget[IO, Event]],
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
