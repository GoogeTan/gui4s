package gui4s.core.widget
package recomposition

def hasNoReactionOnRecomposition[RecompositionReaction](empty : RecompositionReaction) : ReactsOnRecomposition[Any, RecompositionReaction] =
  (_ : Any, _, _) => empty
end hasNoReactionOnRecomposition
