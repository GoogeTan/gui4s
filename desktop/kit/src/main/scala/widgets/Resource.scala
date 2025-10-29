package gui4s.desktop.kit
package widgets

import effects.*
import effects.Update.given
import widgets.decorator.launchedEvent

import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import gui4s.core.widget.Path
import gui4s.desktop.widget.library.{ResourceWidget, WithContext, resourceWidget as genericResourceWidget}

import scala.reflect.Typeable

def resource[IO[_] : MonadThrow, Event](
                                        supervisor : Supervisor[IO],
                                        raiseExternalEvent : DownEvent => IO[Unit]
                                      )(using Typeable[IO[Unit]]) : ResourceWidget[DesktopWidget[IO, Event], IO] =
  genericResourceWidget[
    DesktopWidget[IO, *],
    Update[IO, *, *],
    IO,
    Event
  ](
    transitiveStatefulWidget = transitiveStatefulWidget[IO],
    launchedEffect =
      [TaskEvent : Typeable as TET] => (name, child, task) =>
        launchedEvent[IO, Either[TaskEvent, Event], Unit](
          supervisor, 
          raiseExternalEvent,
          (valueFound : Any) => valueFound match
            case Left[Any, Any](event : TaskEvent) => Some(Left(event))
            case _ => None
        )(
          name,
          child.mapEvent(Right(_)),
          (),
          task.map(Left(_))
        ),
    doubleAllocError = [T] => (path : Path) => Update.raiseError(new Exception("Double resource alloc at " + path.toString))
  )
end resource

def resourceInit[IO[_] : MonadThrow, Event, Value : Typeable](
  supervisor : Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
)(
  name : String,
  init : IO[Value]
)(using Typeable[IO[Unit]]) : WithContext[DesktopWidget[IO, Event], Option[Value]] =
  resource(supervisor, raiseExternalEvent)(name, init.map(value => (value, ().pure[IO])))
end resourceInit

def initWidget[
  IO[_] : MonadThrow,
  Event,
  Value : Typeable
](
  supervisor: Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
)(
  name : String,
  imageSource : IO[Value],
  imageWidget : Value => DesktopWidget[IO, Event],
  placeholder : DesktopWidget[IO, Event],
)(using Typeable[IO[Unit]]) : DesktopWidget[IO, Event] =
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
