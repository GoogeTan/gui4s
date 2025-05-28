package me.katze.gui4s.widget
package recomposition

def hasNoReactionOnRecomposition[RecompositionReaction](empty : RecompositionReaction) : ReactsOnRecomposition[Any, RecompositionReaction] =
  (_ : Any, _, _) => empty
end hasNoReactionOnRecomposition
