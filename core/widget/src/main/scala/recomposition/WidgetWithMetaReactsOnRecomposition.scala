package gui4s.core.widget
package recomposition

def widgetWithMetaReactsOnRecomposition[Widget, Meta, RecompositionReaction](initial : ReactsOnRecomposition[Widget, RecompositionReaction]) : ReactsOnRecomposition[(Widget, Meta), RecompositionReaction] =
  (self, path, states) =>
    initial(self._1, path, states)
end widgetWithMetaReactsOnRecomposition
