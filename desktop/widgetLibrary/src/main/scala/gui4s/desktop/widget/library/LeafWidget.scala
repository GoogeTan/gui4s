package gui4s.desktop.widget.library

import cats.Functor
import cats.Monad

import gui4s.core.widget.free.AsFree
import gui4s.core.widget.handle.handlesNothing
import gui4s.core.widget.merge.anyHasNothingToMerge
import gui4s.core.widget.recomposition.hasNoReactionOnRecomposition
import gui4s.core.widget.state.hasNoInnerState

def leafWidget[
  Marker,
  Update[_] : Monad as M,
  Place[_] : Functor as F,
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
](
   freeMarker : Place[Marker],
   emptyDraw : Draw,
   emptyRecomposition : RecompositionReaction,
 ) : Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]] =
  F.map(freeMarker)(
    marker =>
      val asFree : AsFree[Marker, Place[Marker]] = (_ : Any) => freeMarker
      Widget.ValueWrapper[Marker, Update, Place, Draw, RecompositionReaction, EnvironmentalEvent](
        valueToDecorate = marker,
        valueAsFree = asFree,
        valueIsDrawable = (_ : Marker) => emptyDraw,
        valueHandlesEvent = handlesNothing,
        valueMergesWithOldState = anyHasNothingToMerge,
        valueReactsOnRecomposition = hasNoReactionOnRecomposition[RecompositionReaction](emptyRecomposition),
        valueHasInnerState = hasNoInnerState[Marker]
      )
  )
end leafWidget
