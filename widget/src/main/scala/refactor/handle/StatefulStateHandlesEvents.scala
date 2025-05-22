package me.katze.gui4s.widget
package refactor.handle

import refactor.StatefulState

import cats.Functor
import cats.syntax.functor.*

def StatefulStateHandlesEvents[
  Update[_] : Functor,
  State,
  TaskSupervisor,
  Draw,
  ChildEvent,
  EventReaction,
  Destructor
](
  runReaction : (EventReaction, Path, TaskSupervisor) => Update[State]
) : HandlesEvent[
  StatefulState[State, TaskSupervisor, Draw, (State, List[ChildEvent]) => EventReaction, Destructor], 
  List[ChildEvent], 
  Update[StatefulState[State, TaskSupervisor, Draw, (State, List[ChildEvent]) => EventReaction, Destructor]]
] =
  (self, pathToParent, events) =>
    runReaction(
      self.handleEvents(self.currentState, events), 
      pathToParent, 
      self.taskSupervisor
    ).map(newState => self.copy(currentState = newState))
end StatefulStateHandlesEvents
