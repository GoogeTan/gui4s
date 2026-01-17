package gui4s.android.kit.widgets

import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import cats.effect.{MonadCancel, Resource}
import gui4s.core.widget.Path
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.widgets.decorator.LaunchedEvent
import gui4s.core.widget.library.{WithContext, resourceWidget as genericResourceWidget}

import scala.reflect.Typeable

trait ResourceWidget[IO[_]]:
  def apply[T, Event](
                    name : String, init : Resource[IO, T]
                  ) : WithContext[AndroidWidget[IO, Event], Option[T]]
end ResourceWidget

object ResourceWidget:
  def apply[IO[_]](
                    supervisor : Supervisor[IO],
                    raiseExternalEvent : DownEvent => IO[Unit],
                   )(using Typeable[IO[Unit]], MonadCancel[IO, Throwable]) : ResourceWidget[IO] =
    new ResourceWidget[IO]:
      override def apply[T, Event](name: String, init: Resource[IO, T]) : WithContext[AndroidWidget[IO, Event], Option[T]] =
        body => genericResourceWidget[
          AndroidWidget[IO, *],
          Update[IO, *, *],
          PlaceC[IO],
          IO,
          Event
        ](
          transitiveStatefulWidget = transitiveStatefulWidget[IO],
          launchedEffect =
            [TaskEvent : Typeable as TET] => (name : String, child : AndroidWidget[IO, Event], task : IO[TaskEvent]) =>
              LaunchedEvent[IO, Either[TaskEvent, Event], Unit]( // TODO remove direct dependency
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
        )(name, init.allocated)(body)
      end apply
    end new
  end apply
end ResourceWidget
