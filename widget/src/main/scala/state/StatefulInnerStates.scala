package me.katze.gui4s.widget
package state

import cats.Functor
import cats.syntax.functor.*

def statefulInnerStates[
  State,
  Widget,
  Recomposition
](stateHasInnerState : InnerStates[State, Recomposition]) : InnerStates[Stateful[Widget, State], Recomposition] =
  self => stateHasInnerState(self.state)

