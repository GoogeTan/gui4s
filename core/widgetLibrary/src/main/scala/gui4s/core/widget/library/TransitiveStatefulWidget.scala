package gui4s.core.widget.library

import scala.reflect.Typeable

import catnip.BiMonad
import catnip.syntax.all.*
import cats.Monad
import cats.syntax.all.*

import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.StatefulWidget

trait TransitiveStatefulWidget[Widget[_], -Update[Event, Value], -Destructor[_], -MergeStates[_]]:
  def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                          name: String,
                                                          initialState: State,
                                                          eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *]],
                                                          body: State => Widget[Either[OwnEvent, TransitiveEvent]],
                                                        ): Widget[TransitiveEvent]


  def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                         name: String,
                                                         initialState: State,
                                                         eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *]],
                                                         body: State => Widget[Either[OwnEvent, TransitiveEvent]],
                                                         destructor: Destructor[State],
                                                       ): Widget[TransitiveEvent]

  def apply[State, TransitiveEvent, OwnEvent](
                                                         name: String,
                                                         initialState: State,
                                                         eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
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
                                                                  eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *]],
                                                                  body: State => Widget[Either[OwnEvent, TransitiveEvent]]
                                                                ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, events) => {
        val (ownEvents, transitiveEvents) = events.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> eventHandler(state, ownEvents)
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
    eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *]],
    body: State => Widget[Either[OwnEvent, TransitiveEvent]], 
    destructor: Destructor[State]
  ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, events) => {
        val (ownEvents, transitiveEvents) = events.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> eventHandler(state, ownEvents)
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
    eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
    body: State => Widget[Either[OwnEvent, TransitiveEvent]], 
    destructor: Destructor[State],
    mergeStates : MergeStates[State]
  ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, events) => {
        val (ownEvents, transitiveEvents) = events.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> eventHandler(state, ownEvents)
      },
      body,
      destructor,
      mergeStates
    )
  end apply
end TransitiveStatefulWidgetFromStatefulWidget
