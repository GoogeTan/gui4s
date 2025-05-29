package me.katze.gui4s.widget
package state

import cats.Functor
import cats.syntax.all.*

def statefulStateIsState[
  Merge[_] : Functor,
  State : Equiv as EQ,
  Draw,
  EventHandler,
  RecompositionReaction
] : HasInnerStates[
  StatefulState[State, Draw, EventHandler, State => RecompositionReaction],
  RecompositionReaction
] =
  self =>
    Map(
      self.name -> StateTree((self.initialState, self.currentState), self.destructor(self.currentState), Map())
    )
end statefulStateIsState

