package me.katze.gui4s.example
package api.decorator

import cats.syntax.all.*
import cats.{Functor, Monad, Monoid}
import me.katze.gui4s.layout.{Placed, Rect}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.handle.HandlesEvent
import me.katze.gui4s.widget.library.Widget

/**
 * Декорирует обновление виджета.
 * @param original Декорируемый виджет
 * @param decorator Декоратор
 */
def eventHandleDecorator[
  T,
  Update[+_] : Monad,
  Place[+_] : Functor,
  Draw : Monoid,
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
  Update[+_] : Monad,
  Place[+_] : Functor,
  Draw : Monoid,
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

def eventCatcherWithWidgetsRect[
  T,
  Update[+_] : Monad,
  SimplePlace[+_] : Functor,
  Draw : Monoid,
  RecompositionReaction,
  HandleableEvent,
  MeasurableUnit : Numeric,
](
   markEventHandled : Update[Unit]
 )(
   original : SimplePlace[Placed[MeasurableUnit, Widget[T, Update, [Value] =>> SimplePlace[Placed[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]],
 )(
   decorator : (Path, Rect[MeasurableUnit], MeasurableUnit, HandleableEvent) => Update[Boolean]
 ) : SimplePlace[Placed[MeasurableUnit, Widget[T, Update, [Value] =>> SimplePlace[Placed[MeasurableUnit, Value]], Draw, RecompositionReaction, HandleableEvent]]] =
  given Functor[[Value] =>> SimplePlace[Placed[MeasurableUnit, Value]]] with
    override def map[A, B](value : SimplePlace[Placed[MeasurableUnit, A]])(f : A => B) : SimplePlace[Placed[MeasurableUnit, B]] =
      value.map(_.mapValue(f))
    end map
  end given
  
  original.map(
    placedWidget =>
      placedWidget.mapValue(
        widget =>
          eventCatcher(markEventHandled)(original = widget)(decorator(_, placedWidget.size, placedWidget.z, _))
      )
  )
end eventCatcherWithWidgetsRect