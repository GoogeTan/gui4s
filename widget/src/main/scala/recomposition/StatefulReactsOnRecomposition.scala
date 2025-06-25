package me.katze.gui4s.widget
package recomposition

def statefulReactsOnRecomposition[
  Widget,
  State,
  RecompositionReaction,
](
  widgetReactsOnRecomposition : ReactsOnRecomposition[Widget, RecompositionReaction],
  noRecomposition : RecompositionReaction,
) : ReactsOnRecomposition[Stateful[Widget, State], RecompositionReaction] =
  (self, pathToParent, states) =>
    states.get(self.name).map(_.childrenStates) match
      case Some(oldState) =>
        widgetReactsOnRecomposition(self.child, pathToParent.appendLast(self.name), oldState)
      case None => 
        noRecomposition
end statefulReactsOnRecomposition
