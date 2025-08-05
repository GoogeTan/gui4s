package me.katze.gui4s.widget.library

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.NonEmptyList
import me.katze.gui4s.geometry.RectAtPoint2d
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.handle.HandlesEventF

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

type WithContext[Widget, +Memories] =
  (Memories => Widget) => Widget

given[Widget] : Functor[WithContext[Widget, *]] with
  override def map[A, B](fa: WithContext[Widget, A])(f: A => B): WithContext[Widget, B] =
    widget => fa(f andThen widget)
  end map
end given

def eventMemory[
  Widget[_], 
  Update[_, _],
  MeasurementUnit,
  Event,
  HandlableEvent, 
  Memories : Typeable,
  MemorableEvent,
  T
](
  eventCatcherWithRect: EventCatcherWithRect[Widget[Either[MemorableEvent, Event]], Update[Either[MemorableEvent, Event], T], MeasurementUnit, HandlableEvent],
  statefulWidget: TransitiveStatefulWidget[Widget, Update],
  mapUpdate : [A, B] => (A => B) => Update[A, T] => Update[B, T],
  mapEvent: MapEvent[Widget],
  name : String,
  initialMemories : Memories,
  handleEvent : HandlesEventF[Memories, NonEmptyList[MemorableEvent], Update[Event, *]],
  catchEvent : (Path, RectAtPoint2d[MeasurementUnit], HandlableEvent) => Update[MemorableEvent, T]
) : WithContext[Widget[Event], Memories] =
  widget =>
    statefulWidget[Memories, Event, MemorableEvent](
      name = name,
      initialState = initialMemories,
      eventHandler = handleEvent,
      body =
        memories =>
            eventCatcherWithRect(
              mapEvent.mapEvent(
                widget(memories)
              )(Right(_))
            )((path, rect, handlableEvent) => mapUpdate[MemorableEvent, Either[MemorableEvent, Event]](Left(_))(catchEvent(path, rect, handlableEvent)))
    )
end eventMemory

def rememberLastEventOfTheType[
  Widget[_],
  Update[_, _] : BiMonad as UBM,
  MeasurementUnit,
  Event,
  HandlableEvent,
  MemorableEvent : Typeable,
  T
](
  eventCatcherWithRect: EventCatcherWithRect[Widget[Either[MemorableEvent, Event]], Update[Either[MemorableEvent, Event], T], MeasurementUnit, HandlableEvent],
  statefulWidget: TransitiveStatefulWidget[Widget, Update],
  mapUpdate : [A, B] => (A => B) => Update[A, T] => Update[B, T],
  mapEvent: MapEvent[Widget],
  name : String,
  catchEvent : (Path, RectAtPoint2d[MeasurementUnit], HandlableEvent) => Update[MemorableEvent, T]
) : WithContext[Widget[Event], Option[MemorableEvent]] =
  eventMemory[Memories = Option[MemorableEvent], MemorableEvent = MemorableEvent](
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