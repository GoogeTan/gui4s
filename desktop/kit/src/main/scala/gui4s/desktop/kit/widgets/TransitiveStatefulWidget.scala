package gui4s.desktop.kit
package widgets

import catnip.syntax.additional.*
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.*
import gui4s.desktop.kit.effects.*

import scala.reflect.Typeable
import gui4s.desktop.widget.library.defaultMergeStates

def simpleTransitiveStatefulWidget[
  State : {Equiv, Typeable},
  TransitiveEvent,
  OwnEvent
](
  name : String,
  initialState : State,
  eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
  body : State => DesktopWidget[Either[OwnEvent, TransitiveEvent]],
): DesktopWidget[TransitiveEvent] =
  transitiveStatefulWidget[
    State,
    TransitiveEvent,
    OwnEvent
  ](
    name = name,
    initialState = initialState,
    eventHandler = eventHandler,
    body = body,
    mergeStates = defaultMergeStates,
    destructor = _ => RecompositionReaction.empty
  )
end simpleTransitiveStatefulWidget

def transitiveStatefulWidget[
  State : Typeable,
  TransitiveEvent,
  OwnEvent
](
  name : String,
  initialState : State,
  eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
  body : State => DesktopWidget[Either[OwnEvent, TransitiveEvent]],
  destructor : State => RecompositionReaction,
  mergeStates: MergeStates[Place, State],
): DesktopWidget[TransitiveEvent] =
  gui4s.core.widget.library.transitiveStatefulWidget[
    DesktopWidget,
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
        handler,
        body,
        destructor,
        mergeStates
      ),
    Update.emitEvents,
    eventHandler
  )
end transitiveStatefulWidget
