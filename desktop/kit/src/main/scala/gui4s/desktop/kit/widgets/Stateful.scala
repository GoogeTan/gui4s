package gui4s.desktop.kit
package widgets

import catnip.syntax.all.*
import cats.*
import cats.data.*
import cats.effect.*
import gui4s.core.layout.Sized
import gui4s.core.layout.Sized.given
import gui4s.core.widget.{Path, StatefulState}
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
    initialState = initialState,
    eventHandler = eventHandler,
    body = body,
    mergeStates = defaultMergeStates
  )
end simpleStatefulWidget

def statefulWidget[
  State : Typeable,
  Event,
  ChildEvent
](
  name: String,
  initialState: State,
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
    ChildEvent
  ](
    widgetsAreMergeable = widgetsCanUpdateStateFromTheOldOnes(
      widgetMergesWithOldState,
      widgetHasInnerStates
    ),
    typeCheckState = Place.typecheck[StatefulState[State]](
      (valueToTypeCheck : Any, path : Path) =>  new Exception("Error in stateful typechecking at " + path.toString + " with value [" + valueToTypeCheck.toString + "]")
    ),
    liftUpdate = Update.catchEvents[ChildEvent, Event],
    addNameToPlacePath = Place.addNameToPath,
    addNameToUpdatePath = Update.addNameToPath,
    noRecomposition = RecompositionReaction.empty
  )(
    name = name,
    initialState = initialState,
    handleEvent = eventHandler,
    render = body,
    mergeStates = mergeStates
  )
end statefulWidget
