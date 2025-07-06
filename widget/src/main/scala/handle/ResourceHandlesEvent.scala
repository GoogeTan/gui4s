package me.katze.gui4s.widget
package handle

import catnip.syntax.additional.*
import cats.{Applicative, Functor}
import cats.syntax.all.*
import catnip.syntax.all.{*, given}
import me.katze.gui4s.widget.free.AsFreeF

def resourceHandlesEvent[
  Widget,
  Value,
  HandlableEvent,
  Update[_] : Applicative,
  Place[_] : Functor
](
  widgetHandlesEvent : HandlesEventF[Widget, HandlableEvent, Update * Place],
  resourceAsFree     : AsFreeF[Resource[Widget, Value], Place],
  differentiateEvent : (Path, HandlableEvent) => Either[HandlableEvent, Value]
) : HandlesEventF[
  Resource[Widget, Value],
  HandlableEvent,
  Update * Place
] =
  (self, path, event) =>
    differentiateEvent(path, event) match
      case Left(event) => 
        widgetHandlesEvent(
          self.widget,
          path.appendLast(self.name),
          event
        ).map(newWidget => self.copy(widget = newWidget))
      case Right(newValue) => 
        resourceAsFree(
          self.copy(value = newValue)
        ).pure[Update]
end resourceHandlesEvent
