package gui4s.desktop.widget.library
package decorator

import cats.Comonad
import cats.Functor
import cats.syntax.all.*
import cats.~>


def mapUpdate[
  OldUpdate[_],
  NewUpdate[_] : Functor,
  PlacementEffect[_] : Functor,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
](
  original : FreeWidgetWithSituated[OldUpdate, PlacementEffect, Situated, Draw, RecompositionReaction],
  mapEventInUpdate : OldUpdate ~> NewUpdate
) : FreeWidgetWithSituated[NewUpdate, PlacementEffect, Situated, Draw, RecompositionReaction] =
  trueUpdateDecoratorWithRect(
    original,
    self =>
      mapEventInUpdate(self.extract.handleEvent)
  )
end mapUpdate
