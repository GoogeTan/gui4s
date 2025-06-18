package me.katze.gui4s.widget
package merge

import cats.Functor
import cats.syntax.functor.*

def mergeWithOldStatesStateful[
  State,
  Merge[_] : Functor,
  Widget,
  Recomposition
](
  mergeWithOldInnerStates: MergesWithOldStates[State, Recomposition, Merge[State]]
) : MergesWithOldStates[Stateful[Widget, State], Recomposition, Merge[Stateful[Widget, State]]] =
  (self, pathToParent, innerStates) =>
    mergeWithOldInnerStates(self.state, pathToParent, innerStates)
      .map(newState => self.copy(state = newState))
end mergeWithOldStatesStateful
