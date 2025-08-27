package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*
import widgets.decorator.*

import cats.effect.IO
import cats.effect.std.Supervisor
import gui4s.decktop.widget.library.{resourceWidget as genericResourceWidget, ResourceWidget}
import gui4s.decktop.widget.library.WithContext
import gui4s.core.widget.Path

import scala.reflect.Typeable

def resource[Event](supervisor : Supervisor[IO], raiseExternalEvent : DownEvent => IO[Unit]) : ResourceWidget[DesktopWidget[Event], IO] =
  genericResourceWidget[
    DesktopWidget,
    Update,
    IO,
    Event
  ](
    transitiveStatefulWidget = transitiveStatefulWidget,
    launchedEffect =
      [TaskEvent : Typeable] => (name, child, task) =>
          launchedEvent[Either[TaskEvent, Event], Unit](supervisor, raiseExternalEvent)(
            name,
            child.mapEvent(Right(_)),
            (),
            task.map(Left(_))
          ),
    doubleAllocError = [T] => (path : Path) => Update.raiseError("Double resource alloc at " + path.toString)
  )
end resource

def resourceInit[Event, Value : Typeable](
  supervisor : Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
)(
  name : String,
  init : IO[Value]
) : WithContext[DesktopWidget[Event], Option[Value]] =
  resource(supervisor, raiseExternalEvent)(name, init.map(value => (value, IO.unit)))
end resourceInit

def initWidget[Event, Value](
                              supervisor: Supervisor[IO],
                              raiseExternalEvent : DownEvent => IO[Unit],
                            )(
                              name : String,
                              imageSource : IO[Value],
                              imageWidget : Value => DesktopWidget[Event],
                              placeholder : DesktopWidget[Event],
                            ) : DesktopWidget[Event] =
  resourceInit(
    supervisor,
    raiseExternalEvent,
  )(
    name,
    imageSource
  ) {
    case Some(image) => imageWidget(image)
    case None => placeholder
  }
end initWidget
