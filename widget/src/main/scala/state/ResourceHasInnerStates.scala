package me.katze.gui4s.widget
package state

def resourceHasInnerStates[Widget, Value, RecompositionAction](
                                            hasInnerStates: HasInnerStates[Widget, RecompositionAction],
                                            dealloc : Value => RecompositionAction
                                          ) : HasInnerStates[Resource[Widget, Value], RecompositionAction] =
  resource =>
    Map(
      (
        resource.name,
        StateTree(
          state = resource.value,
          quitCompositionReaction = dealloc(resource.value),
          childrenStates = hasInnerStates(
            resource.widget
          )
        )
      )
    )
end resourceHasInnerStates
