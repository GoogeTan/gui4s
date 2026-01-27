package gui4s.desktop.widget.library

import cats.Functor
import cats.Monad
import cats.syntax.all._

def text[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
  PlacedText
](
    text : Place[PlacedText],
    draw : PlacedText => Draw,
    emptyRecomposition : RecompositionReaction,
) : Place[
  Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]
] =
  drawOnlyWidget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent](text.map(draw), emptyRecomposition)
end text

