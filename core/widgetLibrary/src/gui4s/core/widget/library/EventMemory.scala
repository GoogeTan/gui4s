package gui4s.core.widget.library

import scala.reflect.Typeable

import catnip.BiMonad

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
  Memories,
  MemorableEvent,
  T
](
  eventCatcherWithRect: EventCatcherWithRect[
    Widget[Either[MemorableEvent, Event]], 
    Update[Either[MemorableEvent, Event], T],
    Rect
  ],
  statefulWidget: (
    name : String,
    initialState : Memories,
    eventHandler : HandlesEventF[Memories, List[MemorableEvent], Update[Event, *]],
    body : Memories => Widget[Either[MemorableEvent, Event]]
  ) => Widget[Event],
  mapUpdate : [A, B] => (A => B) => Update[A, T] => Update[B, T],
  mapEvent: MapEvent[Widget],
  name : String,
  initialMemories : Memories,
  handleEvent : HandlesEventF[Memories, List[MemorableEvent], Update[Event, *]],
  catchEvent : Rect => Update[MemorableEvent, T]
) : WithContext[Widget[Event], Memories] =
  widget =>
    statefulWidget(
      name = name,
      initialState = initialMemories,
      eventHandler = handleEvent,
      body =
        memories =>
            eventCatcherWithRect(
              rect =>
                mapUpdate[MemorableEvent, Either[MemorableEvent, Event]](Left(_))(catchEvent(rect))
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
  MemorableEvent,
  T
](
  eventCatcherWithRect: EventCatcherWithRect[Widget[Either[MemorableEvent, Event]], Update[Either[MemorableEvent, Event], T], Rect],
  statefulWidget: (
    name : String,
    initialState : Option[MemorableEvent],
    eventHandler : HandlesEventF[Option[MemorableEvent], List[MemorableEvent], Update[Event, *]],
    body : Option[MemorableEvent] => Widget[Either[MemorableEvent, Event]]
  ) => Widget[Event],
  mapUpdate : [A, B] => (A => B) => Update[A, T] => Update[B, T],
  mapEvent: MapEvent[Widget],
  name : String,
  catchEvent : Rect => Update[MemorableEvent, T]
) : WithContext[Widget[Event], Option[MemorableEvent]] =
  eventMemory[
    Widget, Update, Rect, Event, Option[MemorableEvent], MemorableEvent, T
  ](
    eventCatcherWithRect,
    statefulWidget,
    mapUpdate,
    mapEvent,
    name,
    None,
    (state, events) => UBM().pure(events.lastOption.orElse(state)),
    catchEvent
  )
end rememberLastEventOfTheType

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given[T : Typeable] : Typeable[Option[T]] = (value : Any) => value match
  case a : Option[?] =>
    a match
      case Some(b : T) => Some(Some(b).asInstanceOf[value.type & Option[T]])
      case Some(_) => None
      case None => Some(None.asInstanceOf[value.type & Option[T]])
  case _ => None