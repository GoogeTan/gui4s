package gui4s.core.widget
package state

import cats.Foldable
import cats.syntax.all._

def containerHasInnerStates[
  Widget,
  Collection[_] : Foldable, 
  RecompositionAction
](
  initial : HasInnerStates[Widget, RecompositionAction]
) : HasInnerStates[Collection[Widget], RecompositionAction] =
  children =>
    children.toList.flatMap(initial).toMap
end containerHasInnerStates
