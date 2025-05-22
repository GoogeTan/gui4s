package me.katze.gui4s.widget
package refactor.innerstate

import refactor.Stateful

import cats.Functor
import cats.syntax.functor.*

final class StatefulInnerStates[
  State,
  Place[_] : Functor,
  Widget,
  Recomposition
] (
  stateIsInnerState : InnerStates[State, Place[State], Recomposition]
)extends InnerStates[
  Stateful[Widget, State],
  Place[Stateful[Widget, State]],
  Recomposition
]:
  override def innerStates(self: Stateful[Widget, State]): Map[String, StateTree[Recomposition]] =
    stateIsInnerState.innerStates(self.state)
  end innerStates

  override def mergeWithOldInnerStates(
                                        self: Stateful[Widget, State],
                                        innerStates: Map[String, StateTree[Recomposition]]
                                      ): Place[Stateful[Widget, State]] =
    stateIsInnerState
      .mergeWithOldInnerStates(self.state, innerStates)
      .map(newState => self.copy(state = newState))
  end mergeWithOldInnerStates
end StatefulInnerStates
