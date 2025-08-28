package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*
import widgets.decorator.*

import gui4s.decktop.widget.library.{resourceWidget as genericResourceWidget, ResourceWidget}
import gui4s.decktop.widget.library.WithContext
import gui4s.core.widget.Path

import scala.reflect.Typeable
import zio.*
import zio.interop.catz.*

@SuppressWarnings(Array("org.wartremover.warts.All"))
def resource[Event](supervisor : Supervisor[Unit], raiseExternalEvent : DownEvent => UIO[Unit]) : ResourceWidget[DesktopWidget[Event], Task] =
  genericResourceWidget[
    DesktopWidget,
    Update,
    Task,
    Event
  ](
    transitiveStatefulWidget = transitiveStatefulWidget,
    launchedEffect =
      [TaskEvent : Typeable] => (name : String, child : DesktopWidget[Event], task : Task[TaskEvent]) =>
          launchedEvent[Either[TaskEvent, Event], Unit](supervisor, raiseExternalEvent)(
            name,
            child.mapEvent(Right(_)),
            (),
            task.map(Left(_))
          ),
    doubleAllocError = [T] => (path : Path) => Update.raiseError("Double resource alloc at " + path.toString)
  )
end resource

@SuppressWarnings(Array("org.wartremover.warts.All"))
def resourceInit[Event, Value : Typeable](
  supervisor : Supervisor[Unit],
  raiseExternalEvent : DownEvent => UIO[Unit],
)(
  name : String,
  init : Task[Value]
) : WithContext[DesktopWidget[Event], Option[Value]] =
  resource(supervisor, raiseExternalEvent)(name, init.map(value => (value, ZIO.succeed(()))))
end resourceInit

@SuppressWarnings(Array("org.wartremover.warts.All"))
def initWidget[Event, Value](
                              supervisor: Supervisor[Unit],
                              raiseExternalEvent : DownEvent => UIO[Unit],
                            )(
                              name : String,
                              imageSource : Task[Value],
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
