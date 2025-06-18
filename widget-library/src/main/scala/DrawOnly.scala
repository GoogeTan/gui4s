package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import cats.syntax.all.*
import cats.{Applicative, Functor, Monad}
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.handle.handlesNothing
import me.katze.gui4s.widget.merge.anyHasNothingToMerge
import me.katze.gui4s.widget.recomposition.hasNoReactionOnRecomposition
import me.katze.gui4s.widget.state.hasNoInnerState

def drawOnlyWidget[
  Update[+_] : Monad as M,
  Place[+_] : Functor,
  Merge[+_] : Applicative,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](toDraw : Place[Draw], emptyRecomposition : RecompositionReaction) : Place[Widget_[Update, Place, Merge, Draw, RecompositionReaction, HandleableEvent]] =
  toDraw.map(
    draw =>
      val asFree : AsFree[Draw, Place[Draw]] = (_ : Draw) => toDraw
      Widget[Draw, Update, Place, Merge, Draw, RecompositionReaction, HandleableEvent](
        valueToDecorate = draw,
        valueAsFree = asFree,
        valueIsDrawable = _ => draw,
        valueHandlesEvent = handlesNothing[Draw, HandleableEvent, Update[Place[Draw]]](asFree andThen M.pure),
        valueMergesWithOldState = anyHasNothingToMerge(_.pure[Merge]),
        valueReactsOnRecomposition = hasNoReactionOnRecomposition[RecompositionReaction](emptyRecomposition),
        valueHasInnerState = hasNoInnerState[Draw]
      )
  )
end drawOnlyWidget
