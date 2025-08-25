package gui4s.core.widget
package state

def launchedEffectHasInnerState[Key, Task, Recomposition](
  emptyRecomposition : Recomposition,
) : HasInnerStates[LaunchedEffect[Key, Task], Recomposition] =
  self => Map(
    self.name -> new StateTree(self.key, emptyRecomposition)
  )
end launchedEffectHasInnerState
