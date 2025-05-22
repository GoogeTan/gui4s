package me.katze.gui4s.widget
package refactor.merge

import refactor.StatefulState

import cats.Functor
import cats.syntax.functor.*

def mergeStatefulWithOldStates[
  Merge[_] : Functor,
  State: Equiv as EQ,
  TaskSupervisor,
  Draw,
  EventHandler,
  RecompositionReaction
](
  typeCheckState: Any => Merge[(State, State)]
) : MergesWithOldStates[
  StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction],
  RecompositionReaction,
  Merge[StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction]],
] =
  (self, innerStates) =>
    typeCheckState(
      innerStates(self.name)
    ).map((oldInitialState, oldState) =>
      if EQ.equiv(oldInitialState, self.initialState) then
        self.copy(currentState = oldState) // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
      else
        self
    )
end mergeStatefulWithOldStates
