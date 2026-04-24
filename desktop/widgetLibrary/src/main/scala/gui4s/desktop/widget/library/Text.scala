package gui4s.desktop.widget.library

import cats.Functor
import cats.Monad
import cats.syntax.all.*

def text[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  PlacedText
](
    text : Place[PlacedText],
    draw : PlacedText => Draw,
    emptyRecomposition : RecompositionReaction,
) : Place[
  Widget[Update, Place, Draw, RecompositionReaction]
] =
  drawOnlyWidget[Update, Place, Draw, RecompositionReaction](text.map(draw), emptyRecomposition)
end text

