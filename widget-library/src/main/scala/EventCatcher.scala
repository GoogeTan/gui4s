package me.katze.gui4s.widget.library

import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.katze.gui4s.geometry.{Point3d, RectAtPoint2d}
import me.katze.gui4s.layout.{Sized, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.library.Widget

type EventHandleDecorator[Widget, Update] = (Widget, Update => Update) => Widget

/**
 * Декорирует обновление виджета.
 */
def eventHandleDecorator[
  T,
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : EventHandleDecorator[
  Widget.ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
  HandlesEvent[T, HandleableEvent, Update[Place[T]]]
] =
  (original, decorator) =>
    original.copy(valueHandlesEvent = decorator(original.valueHandlesEvent))
end eventHandleDecorator

/**
 * Ловит события, возможно, поглощая их. Если декоратор вернул true, то событие считается поглощенным.
 * @param markEventHandled Помечает событие как поглощенное
 * @param original Виджет для декорирования
 * @param decorator Декоратор. Возвращает true, если событие поглощено
 */
def eventCatcher[
  T,
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  markEventHandled : Update[Unit]
)(
  original : Widget.ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
)(
    decorator : (Path, HandleableEvent) => Update[Boolean]
) : Widget.ValueWrapper[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
  eventHandleDecorator[T, Update, Place, Draw, RecompositionReaction, HandleableEvent](
    original,
    handler =>
      (state, path, event) =>
        decorator(path, event).ifM(
          markEventHandled
            *> original.valueAsFree(state).pure[Update],
          handler(state, path, event)
        )
  )
end eventCatcher

type EventCatcherWithRect[Widget, Update, MeasurableUnit, HandlableEvent] =
  Widget => ((Path, RectAtPoint2d[MeasurableUnit], HandlableEvent) => Update) => Widget

def eventCatcherWithWidgetsRect[
  Update[_] : Monad,
  OuterPlace[_] : Functor,
  Draw : Monoid,
  RecompositionReaction,
  HandleableEvent,
  MeasurableUnit,
](
    markEventHandled : Update[Unit],
    coordinatesOfTheWidget : Update[Point3d[MeasurableUnit]]
) : EventCatcherWithRect[
  OuterPlace[Sized[MeasurableUnit, Widget[Update, [Value] =>> OuterPlace[Sized[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]],
  Update[Boolean],
  MeasurableUnit,
  HandleableEvent
] =
  original => decorator =>
    original.map(
      placedWidget =>
        placedWidget.mapValue {
          case widget: Widget.ValueWrapper[valueType, Update, [Value] =>> OuterPlace[Sized[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent] =>
            eventCatcher(markEventHandled)(original = widget)(
              (path, event) =>
                coordinatesOfTheWidget.flatMap(point3d =>
                  decorator(path, RectAtPoint2d(placedWidget.size, point3d.projectToXY), event)
                )
            )
        }
    )
end eventCatcherWithWidgetsRect