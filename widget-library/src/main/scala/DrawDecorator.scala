package me.katze.gui4s.widget.library

import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Comonad, Functor, Monad}
import me.katze.gui4s.layout.given

def drawDecorator[
  Update[_] : Functor,
  Place[_] : Functor as PF,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  toDraw : InnerPlace[Draw] => Draw
) : Decorator[Place[InnerPlace[Widget[Update, Place * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]] =
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