package gui4s.desktop.widget.library

import cats.Functor
import cats.Monad
import cats.syntax.all._



def text[
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
  Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]
] =
  drawOnlyWidget[Update, Place, Draw, RecompositionReaction, HandleableEvent](text.map(draw), emptyRecomposition)
end text

