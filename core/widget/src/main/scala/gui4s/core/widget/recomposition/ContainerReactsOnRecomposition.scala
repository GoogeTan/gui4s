package gui4s.core.widget
package recomposition

import cats.Foldable
import cats.Monoid
import cats.syntax.all._

def containerReactsOnRecomposition[
  Widget, 
  Collection[_] : Foldable, 
  RecompositionAction : Monoid
](initial : ReactsOnRecomposition[Widget, RecompositionAction]) : ReactsOnRecomposition[Collection[Widget], RecompositionAction] =
  (children, path, states) =>
    children.foldMap(initial(_, path, states))
end containerReactsOnRecomposition
