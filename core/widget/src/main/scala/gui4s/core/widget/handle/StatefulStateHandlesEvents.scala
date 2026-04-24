package gui4s.core.widget
package handle

import cats.Functor
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
  (self, event) =>
    self
      .handleEvents(self.state.currentState, event)
      .map(self.withNewState)
end statefulStateHandlesEvents
