package me.katze.gui4s.widget.library

import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.katze.gui4s.layout.{Point3d, RectAtPoint2d, Sized, given}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.library.{Widget, Widget_}

/**
 * Декорирует обновление виджета.
 * @param original Декорируемый виджет
 * @param decorator Декоратор
 */
def eventHandleDecorator[
  T,
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
   original : Widget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
   decorator : HandlesEvent[T, HandleableEvent, Update[Place[T]]] => HandlesEvent[T, HandleableEvent, Update[Place[T]]]
) : Widget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
  original.copy(
    valueHandlesEvent = decorator(original.valueHandlesEvent)
  )
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
   original : Widget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent],
)(
    decorator : (Path, HandleableEvent) => Update[Boolean]
) : Widget[T, Update, Place, Draw, RecompositionReaction, HandleableEvent] =
  eventHandleDecorator(
    original,
    (handler) =>
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
  OuterPlace[Sized[MeasurableUnit, Widget_[Update, [Value] =>> OuterPlace[Sized[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]],
  Update[Boolean],
  MeasurableUnit,
  HandleableEvent
] =
  original => decorator =>
    original.map(
      placedWidget =>
        placedWidget.mapValue(
          widget =>
            eventCatcher(markEventHandled)(original = widget)(
              (path, event) =>
                coordinatesOfTheWidget.flatMap(point3d =>
                  decorator(path, RectAtPoint2d(placedWidget.size, point3d.projectToXY), event)
                )
            )
        )
    )
end eventCatcherWithWidgetsRect