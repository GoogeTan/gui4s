package me.katze.gui4s.widget
package refactor.handle

import cats.Functor
import cats.syntax.all.*

def RaiseEventAsEventHandled[
  Update[+_] : Functor,
  Self,
  Event,
  HandlableEvent,
  TransitiveEvent
](
  initial : HandlesEvent[Self, TransitiveEvent, Update[Self]],
  distinct : Event => Either[TransitiveEvent, HandlableEvent],
  raiseEvent : HandlableEvent => Update[Unit]
) : HandlesEvent[Self, Event, Update[Self]] =
  (self: Self, pathToParent: Path, event: Event) => 
    distinct(event) match
      case Left(value) =>
        initial(self, pathToParent, value)
      case Right(value) =>
        raiseEvent(value).as(self)
end RaiseEventAsEventHandled
