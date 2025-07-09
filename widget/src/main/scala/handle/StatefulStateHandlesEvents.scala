package me.katze.gui4s.widget
package handle

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.functor.*

def statefulStateHandlesEvents[
  Update[_] : Functor,
  State,
  Draw,
  Event,
  Destructor
] : HandlesEventF[
  StatefulState[State, Draw, HandlesEventF[State, Event, Update], Destructor],
  Event,
  Update,
] =
  (self, pathToParent, event) =>
    self
      .handleEvents(self.currentState, pathToParent, event)
      .map(newState => self.copy(currentState = newState))
end statefulStateHandlesEvents
