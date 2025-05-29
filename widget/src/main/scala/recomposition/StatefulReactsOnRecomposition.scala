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
) : ReactsOnRecomposition[Stateful[Widget, State], RecompositionReaction] =
  (self, pathToParent, states) =>
      widgetReactsOnRecomposition(self.child, pathToParent.appendLast(self.name), states(self.name).childrenStates)
end statefulReactsOnRecomposition
