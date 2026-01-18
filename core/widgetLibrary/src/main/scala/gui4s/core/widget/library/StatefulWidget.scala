package gui4s.core.widget.library

import scala.reflect.Typeable

import catnip.ContT
import cats.data.NonEmptyList

import gui4s.core.widget.StatefulState
import gui4s.core.widget.handle.HandlesEventF

type MergeStates[Place[_], State] = [T] => (StatefulState[State], StatefulState[State], StatefulState[State] => Place[T]) => Place[T]
type MergeStates2[Place[_], State] =
  (StatefulState[State], StatefulState[State]) => ContT[Place, StatefulState[State]]


// TODO remove me
trait StatefulWidget[Widget[_], Update[Event, Value], -Destructor[_], -MergeStates[_]]:
  def apply[
    State : {Equiv, Typeable},
    Event,
    ChildEvent
  ](
    name : String,
    initialState : State,
    eventHandler : HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
    body : State => Widget[ChildEvent]
  ) : Widget[Event]

  def apply[
    State: {Equiv, Typeable},
    Event,
    ChildEvent
  ](
    name: String,
    initialState: State,
    eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
    body: State => Widget[ChildEvent],
    destructor: Destructor[State]
  ): Widget[Event]


  def apply[
    State,
    Event,
    ChildEvent
  ](
     name: String,
     initialState: State,
     eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], Update[Event, *]],
     body: State => Widget[ChildEvent],
     destructor: Destructor[State],
     mergeStates : MergeStates[State]
   ): Widget[Event]
end StatefulWidget
