package me.katze.gui4s.widget
package recomposition

import cats.Monoid
import cats.syntax.all.*

def containerReactsOnRecomposition[Widget, Layout, RecompositionAction : Monoid](initial : ReactsOnRecomposition[Widget, RecompositionAction]) : ReactsOnRecomposition[Container[Widget, Layout], RecompositionAction] =
  (self, path, states) =>
    self.children.foldMap(initial(_, path, states))
end containerReactsOnRecomposition
