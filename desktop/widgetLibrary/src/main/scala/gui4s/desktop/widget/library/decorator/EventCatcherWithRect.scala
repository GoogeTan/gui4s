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
  OuterPlace[_],
  InnerPlace[_] : Comonad,
  HandleableEvent,
](
   updateDecorator: UpdateDecorator[Update, OuterPlace, InnerPlace[PlaceWidget], HandleableEvent],
   markEventHandled : Update[Unit],
   widgetAsFree : AsFreeF[PlaceWidget, OuterPlace * InnerPlace],
   widgetHandlesEvent : HandlesEventF[PlaceWidget, HandleableEvent, Update * OuterPlace * InnerPlace]
) : EventCatcherWithRect[OuterPlace[InnerPlace[PlaceWidget]], Update[Boolean], InnerPlace[PlaceWidget], HandleableEvent] =
  decorator =>
    updateDecorator(
      (self, path, event) =>
        decorator(path, self, event).ifM(
          markEventHandled *> widgetAsFree(self.extract).pure[Update],
          widgetHandlesEvent(self.extract, path, event)
        )
    )
end eventCatcherWithRect