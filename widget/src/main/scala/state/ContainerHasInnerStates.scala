package me.katze.gui4s.widget
package state

import cats.Foldable
import cats.syntax.all.*

def containerHasInnerStates[Widget, C[_] : Foldable, Layout, RecompositionAction](initial : HasInnerStates[Widget, RecompositionAction]) : HasInnerStates[Container[C[Widget], Layout], RecompositionAction] =
  self =>
    self.children.toList.flatMap(initial).toMap
end containerHasInnerStates
