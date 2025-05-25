package me.katze.gui4s.widget
package recomposition

def hasNoReactionOnRecomposition[A, RecompositionReaction](empty : RecompositionReaction) : ReactsOnRecomposition[A, RecompositionReaction] =
  (_, _, _) => empty
end hasNoReactionOnRecomposition
