package me.katze.gui4s.widget
package recomposition

import cats.Semigroup
import cats.syntax.all.*

def statefulReactsOnRecomposition[
  Widget,
  State,
  RecompositionReaction : Semigroup,
](
  widgetReactsOnRecomposition : ReactsOnRecomposition[Widget, RecompositionReaction],
  stateReactsOnRecomposition : ReactsOnRecomposition[State, RecompositionReaction],
) : ReactsOnRecomposition[Stateful[Widget, State], RecompositionReaction] =
  (self, pathToParent, states) =>
    stateReactsOnRecomposition(self.state, pathToParent, states)  // TODO check if I need to adjust path
      |+| widgetReactsOnRecomposition(self.child, pathToParent.appendLast(self.name), states(self.name).childrenStates)
end statefulReactsOnRecomposition
