package gui4s.core.widget
package recomposition

import cats.Foldable
import cats.Monoid
import cats.syntax.all._

def containerReactsOnRecomposition[Widget, C[_] : Foldable, Layout, RecompositionAction : Monoid](initial : ReactsOnRecomposition[Widget, RecompositionAction]) : ReactsOnRecomposition[Container[C[Widget], Layout], RecompositionAction] =
  (self, path, states) =>
    self.children.foldMap(initial(_, path, states))
end containerReactsOnRecomposition
