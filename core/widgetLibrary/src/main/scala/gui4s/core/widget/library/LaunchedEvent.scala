package gui4s.core.widget.library

import cats.Monad
import cats.syntax.all._

import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.EventCatcherWithRect

/**
 * Виджет, позволяющий при каждом изменении ключа запускать задачу, бросающую события.
 *
 * @param pushEvent Эффект, позволяющий кинуть событие виджету по данному пути.
 * @param catchEvent Функция, которая должна поймать событие, брошенное pushEvent. И пробросить его родительскому виджету.
 * @tparam IO Эффект задачи.
 * @tparam Widget Свободный виджет.
 * @tparam Key Ключ.
 * @tparam Update Эффект обновления дерева виджетов.
 * @tparam Rect Положение виджета на экране.
 * @tparam EnvironmentalEvent Внещнее событие.
 * @tparam Event Событие, бросаемое виджетом.
 */
def launchedEvent[
  IO[_] : Monad,
  Widget,
  Key,
  Update[_, _],
  Rect,
  EnvironmentalEvent,
  Event
](
  launchedEffectWidget: LaunchedEffectWidget[Widget, Key, Path => IO[Unit]],
  eventCatcher : EventCatcherWithRect[Widget, Update[Event, Boolean], Rect, EnvironmentalEvent],
  pushEvent : (Path, Event) => IO[Unit],
  catchEvent : (Path, EnvironmentalEvent) => Update[Event, Boolean]
) : LaunchedEffectWidget[Widget, Key, IO[Event]] =
  (name, widget, key, task) =>
    launchedEffectWidget(
      name,
      eventCatcher(
        (path, _, environmentalEvent) =>
          catchEvent(path, environmentalEvent)
      )(widget),
      key,
      path => task.flatMap(pushEvent(path, _))
    )
end launchedEvent