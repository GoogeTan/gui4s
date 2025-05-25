package me.katze.gui4s.widget
package state

import cats.Functor
import cats.syntax.functor.*

def statefulInnerStates[
  State,
  Widget,
  Recomposition
](stateHasInnerState : HasInnerStates[State, Recomposition]) : HasInnerStates[Stateful[Widget, State], Recomposition] =
  self => stateHasInnerState(self.state)

