package me.katze.gui4s.widget.library
package decorator

import decorator.Decorator

import catnip.syntax.additional.*
import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.syntax.all.*
import cats.{Comonad, Functor, Monad}
import me.katze.gui4s.geometry.*
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.Widget
import me.katze.gui4s.widget.{Path, library}

type EventCatcherWithRect[Widget, Update, Rect, HandlableEvent] =
  ((Path, Rect, HandlableEvent) => Update) => Decorator[Widget]

def eventCatcherWithWidgetsRect[
  Update[_] : Monad,
  OuterPlace[_] : Functor as OPF,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  CoordinatesOfTheWidget,
](
  markEventHandled : Update[Unit],
  coordinatesOfTheWidget : Update[CoordinatesOfTheWidget]
) : EventCatcherWithRect[
  OuterPlace[InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]],
  Update[Boolean],
  InnerPlace[CoordinatesOfTheWidget],
  HandleableEvent
] =
  decorator =>
    updateDecoratorWithRect(
      (self, path, event) =>
        coordinatesOfTheWidget.flatMap(point3d =>
          decorator(path, self.as(point3d), event).ifM(
            markEventHandled *> self.extract.asFree.pure[Update],
            self.extract.handleEvent(path, event)
          )
        ),
    )
end eventCatcherWithWidgetsRect
