package me.katze.gui4s.widget
package recomposition

def statefulReactsOnRecomposition[
  Widget,
  State,
  RecompositionReaction,
](
  widgetReactsOnRecomposition : ReactsOnRecomposition[Widget, RecompositionReaction],
) : ReactsOnRecomposition[Stateful[Widget, State], RecompositionReaction] =
  (self, pathToParent, states) =>
    widgetReactsOnRecomposition(
      self.child,
      pathToParent.appendLast(self.name),
      states.get(self.name).map(_.childrenStates).getOrElse(Map())
    )
end statefulReactsOnRecomposition
