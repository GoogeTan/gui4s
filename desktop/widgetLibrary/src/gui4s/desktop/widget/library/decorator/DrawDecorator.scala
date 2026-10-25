package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all.*
import cats.Comonad
import cats.Functor
import cats.syntax.all.*

import gui4s.core.widget.library.decorator.Decorator

def drawDecorator[
  Update[_] : Functor,
  PlacementEffect[_] : Functor as PF,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
](
  toDraw : Situated[Draw] => Draw
) : Decorator[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction]]]] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]
  original =>
    PF.map(
      original
    )(
      _.coflatMap { placedWidget =>
        Widget.ValueWrapper(
          valueToDecorate = placedWidget,
          valueAsFree = placed => PF.map(placed.extract.asFree)(_.coflatten),
          valueIsDrawable = self => toDraw(self.map(_.draw)),
          valueHandlesEvent = self =>
            self.extract.handleEvent.map(_.map(PF.map(_)(_.coflatten))),
          valueMergesWithOldState = (self, states) =>
            self.extract.mergeWithOldState(states).map(PF.map(_)(_.coflatten)),
          valueReactsOnRecomposition = (self, path, states) =>
            self.extract.reactOnRecomposition(path, states),
          valueHasInnerState =
            self => self.extract.innerStates
        )
      }
    )
end drawDecorator