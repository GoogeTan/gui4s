package gui4s.core.widget
package recomposition

// TODO добавить тесты на добавление имен
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
      pathToParent / self.name,
      states.get(self.name).map(_.childrenStates).getOrElse(Map())
    )
end statefulReactsOnRecomposition
