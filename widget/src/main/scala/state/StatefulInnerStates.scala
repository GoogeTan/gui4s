package me.katze.gui4s.widget
package state

def statefulHasInnerStates[
  State,
  Widget,
  Recomposition
](stateHasInnerState : HasInnerStates[State, Recomposition]) : HasInnerStates[Stateful[Widget, State], Recomposition] =
  self => stateHasInnerState(self.state)

