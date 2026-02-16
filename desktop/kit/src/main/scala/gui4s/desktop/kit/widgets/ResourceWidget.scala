package gui4s.desktop.kit
package widgets

import scala.reflect.Typeable

import cats._
import cats.effect.*
import cats.effect.std.Supervisor

import gui4s.core.widget.Path
import gui4s.core.widget.library.WithContext
import gui4s.core.widget.library.{destructibleResourceWidget => genericResourceWidget}

import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.decorator.LaunchedEvent

trait ResourceWidget:
  def apply[T, Event](name : String, init : Resource[IO, T]) : WithContext[DesktopWidget[Event], Option[T]]
end ResourceWidget

object ResourceWidget:
  def apply(
             supervisor : Supervisor[IO],
             raiseExternalEvent : DownEvent => IO[Unit],
           ) : ResourceWidget =
    new ResourceWidget:
      override def apply[T, Event](name: String, init: Resource[IO, T]) : WithContext[DesktopWidget[Event], Option[T]] =
        body => genericResourceWidget[
          DesktopWidget,
          Update[IO, *, *],
          PlaceC[IO],
          RecompositionReaction[IO],
          IO,
          Event
        ](
          transitiveStatefulWidget = transitiveStatefulWidget,
          launchedEvent =
            [TaskEvent : Typeable as TET] => (name : String, child : DesktopWidget[Event], task : IO[TaskEvent]) =>
              LaunchedEvent[Either[TaskEvent, Event], Unit]( // TODO remove direct dependency
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
          doubleAllocError = [T] => (path : Path) => Update.raiseError(new Exception("Double resource alloc at " + path.toString)),
          emptyDesctructor = RecompositionReaction.empty
        )(name, init.allocated)(body)
      end apply
    end new
  end apply
end ResourceWidget
