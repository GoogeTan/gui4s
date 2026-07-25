package gui4s.android.kit.widgets

import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.widgets.decorator.{LaunchedEvent, destructorWidget}
import gui4s.core.widget.library.{WithContext, destructibleResourceWidget as genericResourceWidget}

import gui4s.desktop.widget.library.defaultMergeStates
import scala.reflect.Typeable

trait ResourceWidget:
  def apply[T : Typeable, Event](
    name : String, init : Resource[IO, T]
  ) : WithContext[AndroidWidget[Event], Option[T]]
end ResourceWidget

object ResourceWidget:
  def apply(
    supervisor : Supervisor[IO],
    raiseExternalEvent : DownEvent => IO[Unit],
  ) : ResourceWidget =
    new ResourceWidget:
      override def apply[T : Typeable, Event](name: String, init: Resource[IO, T]) : WithContext[AndroidWidget[Event], Option[T]] =
        body => genericResourceWidget[
          AndroidWidget,
          Update,
          Place,
          RecompositionReaction,
          IO,
          Event
        ](
          transitiveStatefulWidget = [State: Typeable] => (
            name,
            initialState,
            eventHandler,
            body
          ) =>
            transitiveStatefulWidget[
              Option[(State, RecompositionReaction)],
              Event,
              (State, RecompositionReaction)
            ](
              name,
              initialState,
              (state, events) =>
                eventHandler(state, events).map(_.some),
              body,
              defaultMergeStates
            ),
          destructorWidget = [_] => destructor =>
            destructorWidget(name + "_destructor", destructor),
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
          doubleAllocError = [T] => () => Update.raiseError(path => new Exception("Double resource alloc at " + path.toString)),
        )(name, init.allocated)(body)
      end apply
    end new
  end apply

  def apply(eventBus : QueueSink[IO, DownEvent]) : Init[ResourceWidget] =
    Init.evalResource(
      Supervisor[IO].map(apply(_, eventBus.offer))
    )
  end apply
end ResourceWidget
