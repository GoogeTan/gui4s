package me.katze.gui4s.widget.library
package decorator

import decorator.Decorator

import catnip.syntax.additional.*
import cats.syntax.all.*
import cats.{Comonad, Monad}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.free.AsFreeF
import me.katze.gui4s.widget.handle.HandlesEventF
import me.katze.gui4s.widget.{Path, library}

type EventCatcherWithRect[Widget, Update, Rect, HandlableEvent] =
  ((Path, Rect, HandlableEvent) => Update) => Decorator[Widget]

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