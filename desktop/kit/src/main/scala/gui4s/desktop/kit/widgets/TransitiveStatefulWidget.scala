package gui4s.desktop.kit
package widgets

import cats.Eq
import cats.syntax.all.*
import catnip.syntax.additional.*
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.*
import gui4s.desktop.kit.effects.*

import gui4s.desktop.widget.library.defaultMergeStates
import scala.reflect.Typeable

def simpleTransitiveStatefulWidget[
  State : {Eq, Typeable},
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
    initialState = initialState.pure,
    stateAsFree = _ => None.pure,
    eventHandler = eventHandler,
    body = body,
    mergeStates = defaultMergeStates
  )
end simpleTransitiveStatefulWidget

def transitiveStatefulWidget[
  State : Typeable,
  TransitiveEvent,
  OwnEvent
](
  name : String,
  initialState: PlacementEffect[State],
  stateAsFree : State => PlacementEffect[Option[State]],
  eventHandler: HandlesEventF[State, List[OwnEvent], Update[TransitiveEvent, *] * Option],
  body : State => DesktopWidget[Either[OwnEvent, TransitiveEvent]],
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
        stateAsFree,
        handler,
        body,
        mergeStates
      ),
    Update.emitEvents,
    eventHandler
  )
end transitiveStatefulWidget
