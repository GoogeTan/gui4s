package gui4s.core.widget.library

import cats.Monad
import cats.syntax.all.*
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.EventCatcherWithRect

def launchedEvent[
  IO[_] : Monad,
  Widget,
  Key,
  Update[_, _],
  Rect,
  HandleableEvent,
  Event
](
  launchedEffectWidget: LaunchedEffectWidget[Widget, Key, Path => IO[Unit]],
  eventCatcher : EventCatcherWithRect[Widget, Update[Event, Boolean], Rect, HandleableEvent],
  pushEvent : (Path, Event) => IO[Unit],
  catchEvent : (Path, HandleableEvent) => Update[Event, Boolean]
) : LaunchedEffectWidget[Widget, Key, IO[Event]] =
  (name, widget, key, task) =>
    launchedEffectWidget(
      name,
      eventCatcher(
        (path, _, handleableEvent) =>
          catchEvent(path, handleableEvent)
      )(widget),
      key,
      path => task.flatMap(pushEvent(path, _))
    )
end launchedEvent