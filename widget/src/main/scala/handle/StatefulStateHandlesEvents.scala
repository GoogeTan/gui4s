package me.katze.gui4s.widget
package handle

import cats.Functor
import cats.syntax.functor.*

def statefulStateHandlesEvents[
  Update[_] : Functor,
  State,
  Draw,
  ChildEvent,
  Destructor
] : HandlesEvent[
  StatefulState[State, Draw, (State, Path, List[ChildEvent]) => Update[State], Destructor], 
  List[ChildEvent], 
  Update[StatefulState[State, Draw, (State, Path, List[ChildEvent]) => Update[State], Destructor]]
] =
  (self, pathToParent, events) =>
    self
      .handleEvents(self.currentState, pathToParent, events)
      .map(newState => self.copy(currentState = newState))
end statefulStateHandlesEvents
