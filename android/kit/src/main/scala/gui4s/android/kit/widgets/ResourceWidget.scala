package gui4s.android.kit.widgets

import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import cats.effect.*
import gui4s.core.widget.Path
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.widgets.decorator.LaunchedEvent
import gui4s.core.widget.library.{WithContext, destructibleResourceWidget as genericResourceWidget}

import scala.reflect.Typeable

import cats.effect.IO
import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import cats.effect.*
import gui4s.core.widget.Path
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.widgets.decorator.LaunchedEvent
import gui4s.core.widget.library.{WithContext, destructibleResourceWidget as genericResourceWidget}

import scala.reflect.Typeable

trait ResourceWidget:
  def apply[T, Event](
                    name : String, init : Resource[IO, T]
                  ) : WithContext[AndroidWidget[Event], Option[T]]
end ResourceWidget

object ResourceWidget:
  def apply(
                    supervisor : Supervisor[IO],
                    raiseExternalEvent : DownEvent => IO[Unit],
                   )(using Typeable[IO[Unit]], MonadCancel[IO, Throwable]) : ResourceWidget =
    new ResourceWidget:
      override def apply[T, Event](name: String, init: Resource[IO, T]) : WithContext[AndroidWidget[Event], Option[T]] =
        body => genericResourceWidget[
          AndroidWidget,
          Update[*, *],
          Place,
          RecompositionReaction,
          IO,
          Event
        ](
          transitiveStatefulWidget = transitiveStatefulWidget,
          launchedEvent =
            [TaskEvent : Typeable as TET] => (name : String, child : AndroidWidget[Event], task : IO[TaskEvent]) =>
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

  def apply(eventBus : cats.effect.std.QueueSink[IO, DownEvent]) : Init[ResourceWidget] = 
    Init.evalResource(
      Supervisor[IO].map(apply(_, eventBus.offer))
    )
  end apply
end ResourceWidget
