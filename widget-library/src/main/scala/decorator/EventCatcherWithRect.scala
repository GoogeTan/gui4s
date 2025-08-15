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

// TODO убрать координаты виджета
def eventCatcherWithRect[
  Widget,
  Update[_] : Monad,
  Place[_],
  InnerPlace[_] : Comonad,
  HandleableEvent,
  CoordinatesOfTheWidget
](
  updateDecorator: UpdateDecorator[Update, Place, InnerPlace[Widget], HandleableEvent],
  markEventHandled : Update[Unit],
  coordinatesOfTheWidget : Update[CoordinatesOfTheWidget],
  widgetAsFree : AsFreeF[Widget, Place * InnerPlace],
  widgetHandlesEvent : HandlesEventF[Widget, HandleableEvent, Update * Place * InnerPlace]
) : EventCatcherWithRect[Place[InnerPlace[Widget]], Update[Boolean], InnerPlace[CoordinatesOfTheWidget], HandleableEvent] =
  decorator =>
    updateDecorator(
      (self, path, event) =>
        coordinatesOfTheWidget.flatMap(point3d =>
          decorator(path, self.as(point3d), event).ifM(
            markEventHandled *> widgetAsFree(self.extract).pure[Update],
            widgetHandlesEvent(self.extract, path, event)
          )
        )
    )