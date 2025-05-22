package me.katze.gui4s.widget
package refactor.handle

import catnip.BiMonad
import catnip.syntax.all.*

def MapRaisedEventAtEventHandle[
  Self, 
  Update[+_, +_] : BiMonad,
  HandlableEvent,
  A, 
  B,
](
  initial : HandlesEvent[Self, HandlableEvent, Update[Self, A]],
  f : A => B
) : HandlesEvent[Self, HandlableEvent, Update[Self, B]] =
  (self: Self, pathToParent: Path, event: HandlableEvent) =>
    initial(self, pathToParent, event).mapSecond(f)
end MapRaisedEventAtEventHandle
