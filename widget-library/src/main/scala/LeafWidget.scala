package me.katze.gui4s.widget.library

import cats.{Functor, Monad}
import cats.syntax.all.*
import me.katze.gui4s.widget.free.AsFree
import me.katze.gui4s.widget.handle.handlesNothing
import me.katze.gui4s.widget.merge.anyHasNothingToMerge
import me.katze.gui4s.widget.recomposition.hasNoReactionOnRecomposition
import me.katze.gui4s.widget.state.hasNoInnerState

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
 ) : Place[Widget.ValueWrapper[Marker, Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
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
