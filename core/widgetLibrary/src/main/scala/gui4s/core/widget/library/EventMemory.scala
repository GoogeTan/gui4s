package gui4s.core.widget.library

import scala.reflect.Typeable

import catnip.BiMonad
import cats.data.NonEmptyList

import gui4s.core.widget.Path
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.decorator.EventCatcherWithRect

/**
 * Виджет, помнящий последнее событие заданного типа
 * @todo refactor me, I am too heavy. Soo many params
 * @todo Почему у этого нет реализации в модулях для декстопа и андроида?
 */
def eventMemory[
  Widget[_], 
  Update[_, _],
  Rect,
  Event,
  HandlableEvent, 
  Memories : Typeable,
  MemorableEvent,
  T
](
  eventCatcherWithRect: EventCatcherWithRect[Widget[Either[MemorableEvent, Event]], Update[Either[MemorableEvent, Event], T], Rect, HandlableEvent],
  statefulWidget: TransitiveStatefulWidget[Widget, Update, Nothing, Nothing],
  mapUpdate : [A, B] => (A => B) => Update[A, T] => Update[B, T],
  mapEvent: MapEvent[Widget],
  name : String,
  initialMemories : Memories,
  handleEvent : HandlesEventF[Memories, NonEmptyList[MemorableEvent], Update[Event, *]],
  catchEvent : (Path, Rect, HandlableEvent) => Update[MemorableEvent, T]
) : WithContext[Widget[Event], Memories] =
  widget =>
    statefulWidget[Memories, Event, MemorableEvent](
      name = name,
      initialState = initialMemories,
      eventHandler = handleEvent,
      body =
        memories =>
            eventCatcherWithRect(
              (path, rect, handlableEvent) => mapUpdate[MemorableEvent, Either[MemorableEvent, Event]](Left(_))(catchEvent(path, rect, handlableEvent))
            )(
              mapEvent[Event, Either[MemorableEvent, Event]](Right(_))(
                widget(memories)
              )
            )
    )
end eventMemory

/**
 * Виджет, помнящий последнее событие заданного типа или [[None]]
 * @todo refactor me, I am too heavy. Soo many params
 * @todo Почему у этого нет реализации в модулях для декстопа и андроида?
 */
def rememberLastEventOfTheType[
  Widget[_],
  Update[_, _] : BiMonad as UBM,
  Rect,
  Event,
  HandlableEvent,
  MemorableEvent : Typeable,
  T
](
  eventCatcherWithRect: EventCatcherWithRect[Widget[Either[MemorableEvent, Event]], Update[Either[MemorableEvent, Event], T], Rect, HandlableEvent],
  statefulWidget: TransitiveStatefulWidget[Widget, Update, Nothing, Nothing],
  mapUpdate : [A, B] => (A => B) => Update[A, T] => Update[B, T],
  mapEvent: MapEvent[Widget],
  name : String,
  catchEvent : (Path, Rect, HandlableEvent) => Update[MemorableEvent, T]
) : WithContext[Widget[Event], Option[MemorableEvent]] =
  eventMemory[
    Widget, Update, Rect, Event, HandlableEvent, Option[MemorableEvent], MemorableEvent, T
  ](
    eventCatcherWithRect,
    statefulWidget,
    mapUpdate,
    mapEvent,
    name,
    None,
    (_, _, events) => UBM().pure(Some(events.last)),
    catchEvent
  )

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given[T : Typeable] : Typeable[Option[T]] = (value : Any) => value match
  case a : Option[?] =>
    a match
      case Some(b : T) => Some(Some(b).asInstanceOf[value.type & Option[T]])
      case Some(_) => None
      case None => Some(None.asInstanceOf[value.type & Option[T]])
  case _ => None