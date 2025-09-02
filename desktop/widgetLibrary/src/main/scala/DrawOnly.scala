package gui4s.desktop.widget.library

import cats.syntax.all.*
import cats.{Functor, Monad}
import gui4s.core.widget.free.AsFree
import gui4s.core.widget.handle.handlesNothing
import gui4s.core.widget.merge.anyHasNothingToMerge
import gui4s.core.widget.recomposition.hasNoReactionOnRecomposition
import gui4s.core.widget.state.hasNoInnerState

def drawOnlyWidget[
  Update[_] : Monad as M,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
    toDraw : Place[Draw], 
    emptyRecomposition : RecompositionReaction,
) : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  toDraw.map(
    draw =>
      val asFree : AsFree[Draw, Place[Draw]] = (_ : Draw) => toDraw
      Widget.ValueWrapper[Draw, Update, Place, Draw, RecompositionReaction, HandleableEvent](
        valueToDecorate = draw,
        valueAsFree = asFree,
        valueIsDrawable = identity, 
        valueHandlesEvent = handlesNothing[Draw, HandleableEvent, Update[Place[Draw]]](asFree andThen M.pure),
        valueMergesWithOldState = anyHasNothingToMerge(asFree),
        valueReactsOnRecomposition = hasNoReactionOnRecomposition[RecompositionReaction](emptyRecomposition),
        valueHasInnerState = hasNoInnerState[Draw]
      )
  )
end drawOnlyWidget
