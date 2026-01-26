package gui4s.core.widget
package merge

import cats.Functor
import cats.syntax.all._

import gui4s.core.widget.handle.Layout

def containerMergesWithOldStates[
  Place[_] : Functor,
  Collection[_] : Functor,
  Widget,
  RecompositionAction,
  Meta
](
   initial : MergesWithOldStates[Widget, RecompositionAction, Place[Widget]],
) : MergesWithOldStates[
  Container[Collection[(Widget, Meta)], Layout[Place, Collection, Widget, Meta]],
  RecompositionAction,
  Place[Container[Collection[(Widget, Meta)], Layout[Place, Collection, Widget, Meta]]]
] =
  (self, path, oldStates) =>
      self.layout(
        self.children.map((widget, _) => initial(widget, path, oldStates))
      ).map(mergedChildrenPlaced => self.copy(children = mergedChildrenPlaced))
end containerMergesWithOldStates
