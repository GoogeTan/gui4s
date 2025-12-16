package gui4s.core.widget.library

import cats.data.NonEmptyList
import gui4s.core.widget.handle.HandlesEventF

import scala.reflect.Typeable

trait StatefulWidget[Widget[_], Update[Event, Value], -Destructor[_]]:
  def apply[State : Typeable, Event, ChildEvent](
                                                  name : String,
                                                  initialState : State,
                                                  eventHandler : HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
                                                  body : State => Widget[ChildEvent]
                                                ) : Widget[Event]

  def apply[State: Typeable, Event, ChildEvent](
                                                  name: String,
                                                  initialState: State,
                                                  eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
                                                  body: State => Widget[ChildEvent],
                                                  destructor: Destructor[State]
                                                ): Widget[Event]
