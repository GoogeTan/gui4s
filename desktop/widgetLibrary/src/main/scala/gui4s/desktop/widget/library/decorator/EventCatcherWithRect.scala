package gui4s.desktop.widget.library
package decorator

import catnip.syntax.additional.*
import cats.Comonad
import cats.Monad
import cats.syntax.all.*

import gui4s.core.widget.free.AsFreeF
import gui4s.core.widget.handle.HandlesEventF_
import gui4s.core.widget.library.decorator.EventCatcherWithRect


def eventCatcherWithRect[
  PlacedWidget,
  Update[_] : Monad,
  PlacementEffect[_],
  Situated[_] : Comonad
](
   updateDecorator: UpdateDecorator[Update, PlacementEffect, Situated[PlacedWidget]],
   widgetAsFree : AsFreeF[PlacedWidget, PlacementEffect * Situated],
   widgetHandlesEvent : HandlesEventF_[PlacedWidget, Update * Option * PlacementEffect * Situated]
) : EventCatcherWithRect[
  PlacementEffect[Situated[PlacedWidget]], 
  Update[Unit],
  Situated[PlacedWidget]
] =
  decorator =>
    updateDecorator(
      self =>
        decorator(self) *> widgetHandlesEvent(self.extract)
    )
end eventCatcherWithRect