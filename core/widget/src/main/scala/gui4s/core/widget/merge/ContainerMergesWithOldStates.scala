package gui4s.core.widget
package merge

import catnip.Zip
import catnip.Zip.zip
import cats.Foldable
import cats.Functor
import cats.syntax.all._

def containerMergesWithOldStates[
  Place[_] : Functor,
  Collection[_] : {Functor, Foldable, Zip},
  FreeWidget,
  PositionedWidget,
  RecompositionAction
](
   initial : MergesWithOldStates[PositionedWidget, RecompositionAction, Option[FreeWidget]],
   placeIncrementally : Collection[(PositionedWidget, Option[FreeWidget])] => Place[Collection[PositionedWidget]]
) : MergesWithOldStates[
  Collection[PositionedWidget],
  RecompositionAction,
  Option[Place[Collection[PositionedWidget]]]
] =
  (children, path, oldStates) =>
    val newChildren = children.map(initial(_, path, oldStates))
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
