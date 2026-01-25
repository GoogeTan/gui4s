package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all._
import cats.Comonad
import cats.Functor
import cats.syntax.all._

import gui4s.core.widget.library.decorator.Decorator

def drawDecorator[
  Update[_] : Functor,
  Place[_] : Functor as PF,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  toDraw : Situated[Draw] => Draw
) : Decorator[Place[Situated[Widget[Update, Place * Situated, Draw, RecompositionReaction, HandleableEvent]]]] =
  given Functor[Place * Situated] = nestedFunctorsAreFunctors[Place, Situated]
  original =>
    PF.map(
      original
    )(
      _.coflatMap { placedWidget =>
        Widget.ValueWrapper(
          valueToDecorate = placedWidget,
          valueAsFree = placed => PF.map(placed.extract.asFree)(_.coflatten),
          valueIsDrawable = self => toDraw(self.map(_.draw)),
          valueHandlesEvent = (self, path, event) =>
            self.extract.handleEvent(path, event).map(_.map(_.coflatten)),
          valueMergesWithOldState = (self, path, states) =>
            PF.map(self.extract.mergeWithOldState(path, states))(_.coflatten),
          valueReactsOnRecomposition = (self, path, states) =>
            self.extract.reactOnRecomposition(path, states),
          valueHasInnerState =
            self => self.extract.innerStates
        )
      }
    )
end drawDecorator