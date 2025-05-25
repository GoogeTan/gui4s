package me.katze.gui4s.widget
package state

def launchedEffectHasInnerState[Key, Recomposition](
  emptyRecomposition : Recomposition
) : HasInnerStates[LaunchedEffect[Key, Recomposition], Recomposition] =
  self => Map(
    self.name -> new StateTree(self.key, emptyRecomposition)
  )
end launchedEffectHasInnerState
