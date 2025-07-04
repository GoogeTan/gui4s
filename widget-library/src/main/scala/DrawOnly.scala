package me.katze.gui4s.widget.library

import cats.syntax.all.*
import cats.{Functor, Monad}
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.handle.handlesNothing
import me.katze.gui4s.widget.merge.anyHasNothingToMerge
import me.katze.gui4s.widget.recomposition.hasNoReactionOnRecomposition
import me.katze.gui4s.widget.state.hasNoInnerState

def drawOnlyWidget[
  Update[_] : Monad as M,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
    toDraw : Place[Draw], 
    emptyRecomposition : RecompositionReaction,
 ) : Place[Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  toDraw.map(
    draw =>
      val asFree : AsFree[Draw, Place[Draw]] = (_ : Draw) => toDraw
      Widget[Draw, Update, Place, Draw, RecompositionReaction, HandleableEvent](
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
