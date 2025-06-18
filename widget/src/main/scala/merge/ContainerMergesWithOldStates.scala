package me.katze.gui4s.widget
package merge

import handle.Layout

import cats.{Applicative, Functor}
import cats.syntax.all.*

def containerMergesWithOldStates[
  Place[_] : Functor,
  Merge[_] : Applicative,
  Widget,
  RecompositionAction,
  Meta
](
  initial : MergesWithOldStates[Widget, RecompositionAction, Merge[Place[Widget]]],
) : MergesWithOldStates[
  Container[(Widget, Meta), Layout[Place, Widget, Meta]],
  RecompositionAction,
  Merge[Place[Container[(Widget, Meta), Layout[Place, Widget, Meta]]]]
] =
  (self, path, oldStates) =>
    self.children
      .traverse((widget, _) => initial(widget, path, oldStates))
      .map(mergedChildrenFree =>
        self.layout(
          mergedChildrenFree
        ).map(mergedChildrenPlaced => self.copy(children = mergedChildrenPlaced))
      )
end containerMergesWithOldStates
