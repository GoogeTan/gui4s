package gui4s.decktop.widget.library

import cats.{Functor, Monad}
import gui4s.core.widget.free.AsFree
import gui4s.core.widget.handle.handlesNothing
import gui4s.core.widget.merge.anyHasNothingToMerge
import gui4s.core.widget.recomposition.hasNoReactionOnRecomposition
import gui4s.core.widget.state.hasNoInnerState

import scala.language.experimental.namedTypeArguments

def leafWidget[
  Marker,
  Update[_] : Monad as M,
  Place[_] : Functor as F,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
   freeMarker : Place[Marker],
   emptyDraw : Draw,
   emptyRecomposition : RecompositionReaction,
 ) : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  F.map(freeMarker)(
    marker =>
      val asFree : AsFree[Marker, Place[Marker]] = (_ : Any) => freeMarker
      Widget.ValueWrapper[Marker, Update, Place, Draw, RecompositionReaction, HandleableEvent](
        valueToDecorate = marker,
        valueAsFree = asFree,
        valueIsDrawable = (_ : Marker) => emptyDraw,
        valueHandlesEvent = handlesNothing[Marker, HandleableEvent, Update[Place[Marker]]](asFree andThen M.pure),
        valueMergesWithOldState = anyHasNothingToMerge[Marker, Place[Marker]](asFree),
        valueReactsOnRecomposition = hasNoReactionOnRecomposition[RecompositionReaction](emptyRecomposition),
        valueHasInnerState = hasNoInnerState[Marker]
      )
  )
end leafWidget
