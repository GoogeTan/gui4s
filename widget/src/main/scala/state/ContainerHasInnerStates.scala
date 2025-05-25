package me.katze.gui4s.widget
package state

def containerHasInnerStates[Widget, Layout, RecompositionAction](initial : HasInnerStates[Widget, RecompositionAction]) : HasInnerStates[Container[Widget, Layout], RecompositionAction] =
  self =>
    self.children.flatMap(initial).toMap
end containerHasInnerStates
