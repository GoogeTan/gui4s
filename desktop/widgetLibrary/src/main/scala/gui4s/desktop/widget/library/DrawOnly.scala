package gui4s.desktop.widget.library

import catnip.syntax.all._
import cats._
import cats.syntax.all._

import gui4s.core.widget.free.AsFree
import gui4s.core.widget.handle.handlesNothing_
import gui4s.core.widget.merge.anyHasNothingToMerge
import gui4s.core.widget.recomposition.hasNoReactionOnRecomposition
import gui4s.core.widget.state.hasNoInnerState

def drawOnlyWidget[
  Update[_] : Monad as M,
  Place[_] : Functor,
  Draw,
  RecompositionReaction
](
    toDraw : Place[Draw], 
    emptyRecomposition : RecompositionReaction,
) : Place[Widget[Update, Place, Draw, RecompositionReaction]] =
  toDraw.map(
    draw =>
      val asFree : AsFree[Draw, Place[Draw]] = (_ : Draw) => toDraw
      Widget.ValueWrapper[Draw, Update, Place, Draw, RecompositionReaction](
        valueToDecorate = draw,
        valueAsFree = asFree,
        valueIsDrawable = identity, 
        valueHandlesEvent = handlesNothing_,
        valueMergesWithOldState = anyHasNothingToMerge,
        valueReactsOnRecomposition = hasNoReactionOnRecomposition[RecompositionReaction](emptyRecomposition),
        valueHasInnerState = hasNoInnerState[Draw]
      )
  )
end drawOnlyWidget

def constanctSizeDrawOnlyWidget[
  Update[_] : Monad as M,
  PlacementEffect[_] : Applicative,
  Situated[_] : Functor,
  Draw,
  RecompositionReaction,
](
   toDraw : Situated[Draw],
   emptyRecomposition : RecompositionReaction,
) : PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction]]] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]
  drawOnlyWidget[
    Update,
    PlacementEffect * Situated,
    Draw,
    RecompositionReaction,
](toDraw.pure[PlacementEffect], emptyRecomposition)
end constanctSizeDrawOnlyWidget
