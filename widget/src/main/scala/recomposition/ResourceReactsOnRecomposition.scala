package me.katze.gui4s.widget
package recomposition

import cats.Semigroup
import cats.syntax.all.*

def resourceReactsOnRecomposition[Widget, Value, RecompositionReaction : Semigroup](
                                                                                      startResourceAllocation : Path => RecompositionReaction,
                                                                                      widgetReactsOnRecomposition : ReactsOnRecomposition[Widget, RecompositionReaction],
                                                                                    ) : ReactsOnRecomposition[Resource[Widget, Value], RecompositionReaction] =
  (self, pathToParent, states) =>
    val pathToSelf = pathToParent.appendLast(self.name)
    states.get(self.name) match
      case Some(oldValue) =>
        widgetReactsOnRecomposition(self.widget, pathToSelf, oldValue.childrenStates)
      case None =>
        startResourceAllocation(pathToSelf) |+| widgetReactsOnRecomposition(self.widget, pathToSelf, Map())
end resourceReactsOnRecomposition
