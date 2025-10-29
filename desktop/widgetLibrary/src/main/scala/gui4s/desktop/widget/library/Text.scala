package gui4s.desktop.widget.library

import cats.syntax.all.*
import cats.{Functor, Monad}



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

