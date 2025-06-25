package me.katze.gui4s.widget.library

import cats.data.NonEmptyList
import me.katze.gui4s.widget.EventReaction

import scala.reflect.Typeable

trait StatefulWidget[Widget[_], Task]:
  def apply[State : Typeable, Event, ChildEvent](
                                                  name : String,
                                                  initialState : State,
                                                  eventHandler : (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Task],
                                                  body : State => Widget[ChildEvent]
                                                ) : Widget[Event]

