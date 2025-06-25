package me.katze.gui4s.widget.library

import cats.data.NonEmptyList
import me.katze.gui4s.widget.EventReaction

import scala.reflect.Typeable

trait TransitiveStatefulWidget[Widget[_], Task]:
  def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                         name: String,
                                                         initialState: State,
                                                         eventHandler: (State, NonEmptyList[OwnEvent]) => EventReaction[State, TransitiveEvent, Task],
                                                         body: State => Widget[Either[OwnEvent, TransitiveEvent]]
                                                      ): Widget[TransitiveEvent]

final class TransitiveStatefulWidgetFromStatefulWidget[Widget[_], Task](statefulWidget: StatefulWidget[Widget, Task]) extends TransitiveStatefulWidget[Widget, Task]:
  override def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                                  name: String,
                                                                  initialState: State,
                                                                  eventHandler: (State, NonEmptyList[OwnEvent]) => EventReaction[State, TransitiveEvent, Task],
                                                                  body: State => Widget[Either[OwnEvent, TransitiveEvent]]
                                                                ): Widget[TransitiveEvent] =
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, events) => {
        val (ownEvents, transitiveEvents) = events.toList.partitionMap(identity)
        val ownEventResult =
          NonEmptyList.fromList(ownEvents).map(eventHandler(state, _)).getOrElse(EventReaction(state, Nil, Nil))
        println(transitiveEvents.mkString(", "))
        println(ownEvents.mkString(", "))
        ownEventResult.copy(parentEvent = transitiveEvents ++ ownEventResult.parentEvent)
      },
      body
    )
  end apply
end TransitiveStatefulWidgetFromStatefulWidget
 
  
