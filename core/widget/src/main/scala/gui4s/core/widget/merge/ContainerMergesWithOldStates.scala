package gui4s.core.widget
package merge

import catnip.Zip
import catnip.Zip.zip
import cats.syntax.all.*
import cats.{Foldable, Functor}

def containerMergesWithOldStates[
  Place[_] : Functor,
  Collection[_] : {Functor, Foldable, Zip},
  Widget,
  RecompositionAction,
  Meta
](
   initial : MergesWithOldStates[Widget, RecompositionAction, Option[Place[Widget]]],
   placeIncrementally : Collection[((Widget, Meta), Option[Place[Widget]])] => Place[Collection[(Widget, Meta)]]
) : MergesWithOldStates[
  Collection[(Widget, Meta)],
  RecompositionAction,
  Option[Place[Collection[(Widget, Meta)]]]
] =
  (children, path, oldStates) =>
    val newChildren = children.map((widget, _) => initial(widget, path, oldStates))
    if newChildren.exists(_.isDefined) then
      Some(
        placeIncrementally(
          children.zip(newChildren)
        )
      )
    else
      None
    end if
end containerMergesWithOldStates
