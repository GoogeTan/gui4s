package gui4s.core.widget.library

import catnip.BiMonad
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*
import gui4s.core.widget.StatefulState
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.StatefulWidget

import scala.reflect.Typeable

trait TransitiveStatefulWidget[Widget[_], -Update[Event, Value], -Destructor[_], -MergeStates[_]]:
  def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                          name: String,
                                                          initialState: State,
                                                          eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                          body: State => Widget[Either[OwnEvent, TransitiveEvent]],
                                                        ): Widget[TransitiveEvent]


  def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                         name: String,
                                                         initialState: State,
                                                         eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                         body: State => Widget[Either[OwnEvent, TransitiveEvent]],
                                                         destructor: Destructor[State],
                                                       ): Widget[TransitiveEvent]

  def apply[State, TransitiveEvent, OwnEvent](
                                                         name: String,
                                                         initialState: State,
                                                         eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                         body: State => Widget[Either[OwnEvent, TransitiveEvent]],
                                                         destructor: Destructor[State],
                                                         mergeStates : MergeStates[State]
                                                       ): Widget[TransitiveEvent]
end TransitiveStatefulWidget

final class TransitiveStatefulWidgetFromStatefulWidget[
  Widget[_],
  Update[Event, Value] : BiMonad as updateBiMonad,
  Destructor[_],
  MergeStates[_]
](
  statefulWidget: StatefulWidget[Widget, Update, Destructor, MergeStates],
  raiseEvents : [Event] => List[Event] => Update[Event, Unit],
) extends TransitiveStatefulWidget[Widget, Update, Destructor, MergeStates]:
  override def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                                  name: String,
                                                                  initialState: State,
                                                                  eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                                  body: State => Widget[Either[OwnEvent, TransitiveEvent]]
                                                                ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, path, events) => {
        val (ownEvents, transitiveEvents) = events.toList.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> NonEmptyList
              .fromList(ownEvents)
              .map(eventHandler(state, path, _))
              .getOrElse(state.pure)
      },
      body
    )
  end apply

  override def apply[
    State: Typeable, 
    TransitiveEvent, 
    OwnEvent
  ](
    name: String,
    initialState: State,
    eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
    body: State => Widget[Either[OwnEvent, TransitiveEvent]], 
    destructor: Destructor[State]
  ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, path, events) => {
        val (ownEvents, transitiveEvents) = events.toList.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> NonEmptyList
          .fromList(ownEvents)
          .map(eventHandler(state, path, _))
          .getOrElse(state.pure)
      },
      body,
      destructor,
    )
  end apply  

  override def apply[
    State,
    TransitiveEvent,
    OwnEvent
  ](
    name: String, 
    initialState: State,
    eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
    body: State => Widget[Either[OwnEvent, TransitiveEvent]], 
    destructor: Destructor[State],
    mergeStates : MergeStates[State]
  ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, path, events) => {
        val (ownEvents, transitiveEvents) = events.toList.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> NonEmptyList
          .fromList(ownEvents)
          .map(eventHandler(state, path, _))
          .getOrElse(state.pure)
      },
      body,
      destructor,
      mergeStates
    )
  end apply
end TransitiveStatefulWidgetFromStatefulWidget
