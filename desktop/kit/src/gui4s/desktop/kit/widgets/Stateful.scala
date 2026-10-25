package gui4s.desktop.kit
package widgets

import catnip.syntax.all.*
import catnip.CodensityT
import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.*
import gui4s.core.layout.Sized
import gui4s.core.layout.Sized.given
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.widget.library
import gui4s.desktop.widget.library.*

import scala.reflect.Typeable

def simpleStatefulWidget[
  State : {Eq, Typeable},
  Event,
  ChildEvent
](
  name: String,
  initialState: State,
  eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event] * Option],
  body: State => DesktopWidget[ChildEvent]
): DesktopWidget[Event] =
  statefulWidget[
    State,
    Event,
    ChildEvent
  ](
    name = name,
    initialState = initialState.pure,
    stateAsFree = _ => None.pure,
    eventHandler = eventHandler,
    body = body,
    mergeStates = Place.defaultMergeStates
  )
end simpleStatefulWidget

def statefulWidget[
  State,
  Event,
  ChildEvent
](
  name: String,
  initialState: PlacementEffect[State],
  stateAsFree : State => PlacementEffect[Option[State]],
  eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event] * Option],
  body: State => DesktopWidget[ChildEvent],
  mergeStates: MergeStates[Place, State],
): DesktopWidget[Event] =
  library.stateful[
    UpdateC[Event],
    UpdateC[ChildEvent],
    Place,
    Draw,
    RecompositionReaction,
    State,
    List[ChildEvent]
  ](
    widgetsAreMergeable = widgetsCanUpdateStateFromTheOldOnes(
      widgetMergesWithOldState,
      widgetHasInnerStates
    ),
    liftUpdate = Update.catchEvents[ChildEvent, Event],
    addNameToPlacePath = Place.addNameToPath,
    addNameToUpdatePath = Update.addNameToPath,
    noRecomposition = RecompositionReaction.empty
  )(
    name = name,
    initialState = CodensityT([T] => callback => initialState.flatMap(callback)),
    stateAsFree = oldState => CodensityT([T] => callback => stateAsFree(oldState).flatMap(callback)),
    handleEvent = eventHandler,
    render = body,
    mergeStates = mergeStates
  )
end statefulWidget
