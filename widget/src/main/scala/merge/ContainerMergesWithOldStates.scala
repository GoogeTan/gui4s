package me.katze.gui4s.widget
package merge

import handle.Layout

import cats.Functor
import cats.syntax.all.*

def containerMergesWithOldStates[
  Place[_] : Functor,
  Widget,
  RecompositionAction,
  Meta
](
   initial : MergesWithOldStates[Widget, RecompositionAction, Place[Widget]],
) : MergesWithOldStates[
  Container[(Widget, Meta), Layout[Place, Widget, Meta]],
  RecompositionAction,
  Place[Container[(Widget, Meta), Layout[Place, Widget, Meta]]]
] =
  (self, path, oldStates) =>
      self.layout(
        self.children.map((widget, _) => initial(widget, path, oldStates))
      ).map(mergedChildrenPlaced => self.copy(children = mergedChildrenPlaced))
end containerMergesWithOldStates
