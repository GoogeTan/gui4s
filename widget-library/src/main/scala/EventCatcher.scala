package me.katze.gui4s.widget.library

import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.layout.{Sized, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.{Path, library}
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.library.Widget

type EventHandleDecorator[Widget, Update] = (Widget, Update) => Widget

/**
 * Декорирует обновление виджета.
 * TODO проверить, как оно работает с asFree. Есть впечатление, что это сбросит эффект. Это относится ко всем декораторам.
 */
def eventHandleDecorator[
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : EventHandleDecorator[
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
  [T] => HandlesEvent[T, HandleableEvent, Update[Place[T]]] => HandlesEvent[T, HandleableEvent, Update[Place[T]]]
] =
  (original, decorator) =>
    val originalAsWrapper = original.asWrapper
    originalAsWrapper.copy(valueHandlesEvent = decorator(originalAsWrapper.valueHandlesEvent))
end eventHandleDecorator

/**
 * Декорирует обновление виджета. Полиморфно по отношению к типу состояния.
 */
def eventHandleDecorator_[
  Update[_] : Functor as UF,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](mark : String): EventHandleDecorator[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
  [T] => HandlesEvent[T, HandleableEvent, Update[Place[T]]] => HandlesEvent[T, HandleableEvent, Update[Place[T]]]
] =
  (original, decorator) =>
    basicDecorator[Update, Place, Draw, RecompositionReaction, HandleableEvent](
      mark,
      original,
      eventHandleDecorator(using UF, PF)(_, decorator)
    )
end eventHandleDecorator_

/**
 * Ловит события, возможно, поглощая их. Если декоратор вернул true, то событие считается поглощенным.
 * @param markEventHandled Помечает событие как поглощенное
 * @param original Виджет для декорирования
 * @param decorator Декоратор. Возвращает true, если событие поглощено
 */
def eventCatcher[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  markEventHandled : Update[Unit]
)(
  original : Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
)(
  decorator : (Path, HandleableEvent) => Update[Boolean]
) : Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent] =
  val originalAsWrapper = original.asWrapper
  originalAsWrapper.copy(
    valueHandlesEvent = (state, path, event) =>
        decorator(path, event).ifM(
          markEventHandled *> originalAsWrapper.valueAsFree(state).pure[Update],
          originalAsWrapper.valueHandlesEvent(state, path, event)
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
    basicDecoratorWithRect(
      "event handler",
      original,
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