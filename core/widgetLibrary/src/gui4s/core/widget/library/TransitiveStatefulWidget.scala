package gui4s.core.widget.library

import catnip.syntax.all.*
import cats.Monad
import cats.syntax.all.*
import gui4s.core.widget.handle.HandlesEventF

def transitiveStatefulWidget[
  Widget[_],
  Update[Value] : Monad,
  PlacementEffect[_],
  State,
  OwnEvent,
  TransitiveEvent
](
  statefulWidget : (
    eventHandler: HandlesEventF[State, List[Either[OwnEvent, TransitiveEvent]], Update * Option],
  ) => Widget[TransitiveEvent],
  raiseEvents : List[TransitiveEvent] => Update[Unit],
  eventHandler: HandlesEventF[State, List[OwnEvent], Update * Option],
) : Widget[TransitiveEvent] =
  statefulWidget(
    (state, events) =>
      val (ownEvents, transitiveEvents) = events.partitionMap(identity)
      raiseEvents(transitiveEvents)
        *> eventHandler(state, ownEvents),
  )
end transitiveStatefulWidget