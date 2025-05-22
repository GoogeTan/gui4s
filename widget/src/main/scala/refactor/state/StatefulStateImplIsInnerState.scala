package me.katze.gui4s.widget
package refactor.state

import refactor.StatefulState

import cats.Functor
import cats.syntax.all.*

def statefulStateIsState[
  Merge[_] : Functor,
  State : Equiv as EQ,
  TaskSupervisor,
  Draw,
  EventHandler,
  RecompositionReaction
](
  typeCheckState : Any => Merge[(State, State)]
) : InnerStates[
  StatefulState[State, TaskSupervisor, Draw, EventHandler, State => RecompositionReaction],
  RecompositionReaction
] =
  self =>
    Map(
      self.name -> StateTree((self.initialState, self.currentState), self.destructor(self.currentState), Map())
    )
end statefulStateIsState

