package me.katze.gui4s.widget
package refactor.merge

import refactor.Stateful

import cats.Functor
import cats.syntax.functor.*

def mergeWithOldStatesStateful[
  State,
  Place[_] : Functor,
  Widget,
  Recomposition
](
  mergeWithOldInnerStates: MergesWithOldStates[State, Recomposition, Place[State]]
) : MergesWithOldStates[Stateful[Widget, State], Recomposition, Place[Stateful[Widget, State]]] =
  (self, innerStates) =>
    mergeWithOldInnerStates(self.state, innerStates)
      .map(newState => self.copy(state = newState))
end mergeWithOldStatesStateful
