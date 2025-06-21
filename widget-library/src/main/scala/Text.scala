package me.katze.gui4s.widget.library

import cats.{Applicative, Functor, Monad}
import cats.syntax.all.*

def skijaText[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  PlacedText
](
    text : Place[PlacedText],
    draw : PlacedText => Draw,
    emptyRecomposition : RecompositionReaction,
 ) : Place[
  Widget_[Update, Place, Draw, RecompositionReaction, HandleableEvent]
] =
  drawOnlyWidget[Update, Place, Draw, RecompositionReaction, HandleableEvent](text.map(draw), emptyRecomposition)
end skijaText

