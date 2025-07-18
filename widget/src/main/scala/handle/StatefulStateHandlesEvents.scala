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
  StatefulBehaviour[State, Draw, HandlesEventF[State, Event, Update], Destructor],
  Event,
  Update,
] =
  (self, pathToParent, event) =>
    self
      .handleEvents(self.state.currentState, pathToParent, event)
      .map(self.withNewState)
end statefulStateHandlesEvents
