package gui4s.desktop.widget.library
package decorator

import cats.Comonad
import cats.Functor
import cats.syntax.all._
import cats.~>


def mapUpdate[
  OldUpdate[_],
  NewUpdate[_] : Functor,
  PlacementEffect[_] : Functor,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
](
  original : FreeWidgetWithSituated[OldUpdate, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent],
  mapEventInUpdate : OldUpdate ~> NewUpdate
) : FreeWidgetWithSituated[NewUpdate, PlacementEffect, Situated, Draw, RecompositionReaction, EnvironmentalEvent] =
  trueUpdateDecoratorWithRect(
    original,
    (self, path, event) =>
      mapEventInUpdate(self.extract.handleEvent(path, event))
  )
end mapUpdate
