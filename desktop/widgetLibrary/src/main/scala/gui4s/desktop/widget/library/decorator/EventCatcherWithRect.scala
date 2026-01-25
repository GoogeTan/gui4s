package gui4s.desktop.widget.library
package decorator

import catnip.syntax.additional._
import cats.Comonad
import cats.Monad
import cats.syntax.all._

import gui4s.core.widget.free.AsFreeF
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.decorator.EventCatcherWithRect


def eventCatcherWithRect[
  PlaceWidget,
  Update[_] : Monad,
  PlacementEffect[_],
  Situated[_] : Comonad,
  HandleableEvent,
](
   updateDecorator: UpdateDecorator[Update, PlacementEffect, Situated[PlaceWidget], HandleableEvent],
   markEventHandled : Update[Unit],
   widgetAsFree : AsFreeF[PlaceWidget, PlacementEffect * Situated],
   widgetHandlesEvent : HandlesEventF[PlaceWidget, HandleableEvent, Update * PlacementEffect * Situated]
) : EventCatcherWithRect[PlacementEffect[Situated[PlaceWidget]], Update[Boolean], Situated[PlaceWidget], HandleableEvent] =
  decorator =>
    updateDecorator(
      (self, path, event) =>
        decorator(path, self, event).ifM(
          markEventHandled *> widgetAsFree(self.extract).pure[Update],
          widgetHandlesEvent(self.extract, path, event)
        )
    )
end eventCatcherWithRect