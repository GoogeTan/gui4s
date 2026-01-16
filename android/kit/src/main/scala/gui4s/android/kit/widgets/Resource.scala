package gui4s.android.kit.widgets

import gui4s.core.widget.Path
import gui4s.core.widget.library.{ResourceWidget, WithContext, resourceWidget as genericResourceWidget}
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.widgets.decorator.launchedEvent

import scala.reflect.Typeable

def resource[IO[_] : MonadThrow, Event](
                                        supervisor : Supervisor[IO],
                                        raiseExternalEvent : DownEvent => IO[Unit]
                                      )(using Typeable[IO[Unit]]) : ResourceWidget[AndroidWidget[IO, Event], IO] =
  genericResourceWidget[
    AndroidWidget[IO, *],
    Update[IO, *, *],
    PlaceC[IO],
    IO,
    Event
  ](
    transitiveStatefulWidget = transitiveStatefulWidget[IO],
    launchedEffect =
      [TaskEvent : Typeable as TET] => (name : String, child : AndroidWidget[IO, Event], task : IO[TaskEvent]) =>
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
)(using Typeable[IO[Unit]]) : WithContext[AndroidWidget[IO, Event], Option[Value]] =
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
  imageWidget : Value => AndroidWidget[IO, Event],
  placeholder : AndroidWidget[IO, Event],
)(using Typeable[IO[Unit]]) : AndroidWidget[IO, Event] =
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
