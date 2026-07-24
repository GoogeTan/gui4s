package gui4s.android.kit.widgets

import gui4s.core.widget.library.*
import gui4s.android.kit.effects.*
import gui4s.core.widget.handle.HandlesEventF
import scala.reflect.Typeable

def simpleTransitiveStatefulWidget[
  State : {Eq, Typeable},
  TransitiveEvent,
  OwnEvent
](
  name : String,
  initialState : State,
  eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
  body : State => AndroidWidget[Either[OwnEvent, TransitiveEvent]],
): AndroidWidget[TransitiveEvent] =
  transitiveStatefulWidget[
    State,
    TransitiveEvent,
    OwnEvent
  ](
    name = name,
    initialState = initialState.pure,
    stateAsFree = _ => None.pure,
    eventHandler = eventHandler,
    body = body,
    mergeStates = Place.defaultMergeStates
  )
end simpleTransitiveStatefulWidget

def transitiveStatefulWidget[
  State,
  TransitiveEvent,
  OwnEvent
](
  name : String,
  initialState: PlacementEffect[State],
  stateAsFree : State => PlacementEffect[Option[State]],
  eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
  body : State => AndroidWidget[Either[OwnEvent, TransitiveEvent]],
  mergeStates: MergeStates[Place, State],
): AndroidWidget[TransitiveEvent] =
  gui4s.core.widget.library.transitiveStatefulWidget[
    AndroidWidget,
    Update[TransitiveEvent, *],
    PlacementEffect,
    State,
    OwnEvent,
    TransitiveEvent
  ](
    handler =>
      statefulWidget[
        State,
        TransitiveEvent,
        Either[OwnEvent, TransitiveEvent]
      ](
        name,
        initialState,
        stateAsFree,
        handler,
        body,
        mergeStates
      ),
    Update.emitEvents,
    eventHandler
  )
end transitiveStatefulWidget
