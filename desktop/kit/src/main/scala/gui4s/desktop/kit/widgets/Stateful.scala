package gui4s.desktop.kit
package widgets

import scala.reflect.Typeable

import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*
import cats.effect.*

import gui4s.core.layout.Sized.given
import gui4s.core.widget.Path
import gui4s.core.widget.StatefulState
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.*
import gui4s.core.widget.library.StatefulWidget

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.widget.library
import gui4s.desktop.widget.library.*


def statefulWidget: StatefulWidget[
  DesktopWidget,
  Update,
  * => RecompositionReaction,
  MergeStates[Place, *]
] =
  new StatefulWidget[
    DesktopWidget,
    Update,
    * => RecompositionReaction,
    MergeStates[Place, *]
  ]:
    override def apply[State: {Equiv, Typeable}, Event, ChildEvent](
                                                                      name: String,
                                                                      initialState: State,
                                                                      eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event]],
                                                                      body: State => DesktopWidget[ChildEvent]
                                                                    ): DesktopWidget[Event] =
      apply(name, initialState, eventHandler, body, _ => RecompositionReaction.empty)
    end apply

    @SuppressWarnings(Array("org.wartremover.warts.ToString"))
    override def apply[State: {Equiv as EQ, Typeable}, Event, ChildEvent](
                                                                      name: String,
                                                                      initialState: State,
                                                                      eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event]],
                                                                      body: State => DesktopWidget[ChildEvent],
                                                                      destructor: State => RecompositionReaction
                                                                    ): DesktopWidget[Event] =
      apply[State, Event, ChildEvent](
        name = name,
        initialState = initialState,
        eventHandler =
          (originalState, events) =>
            eventHandler(originalState, events).map(
              resultingState =>
                if EQ.equiv(originalState, resultingState) then
                  None
                else
                  Some(resultingState)
            ),
        body = body,
        destructor = destructor,
        mergeStates = [T] => (oldState, newState, consumer) =>
          if EQ.equiv(oldState.initialState, newState.initialState) then
             Some(consumer(oldState))  // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
          else
             None
      )
    end apply

    override def apply[State, Event, ChildEvent](
                                                  name: String,
                                                  initialState: State,
                                                  eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event] * Option],
                                                  body: State => DesktopWidget[ChildEvent],
                                                  destructor: State => RecompositionReaction,
                                                  mergeStates: MergeStates[Place, State]
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
        addNameToUpdatePath = Update.addNameToPath
      )(
        name = name,
        initialState = initialState,
        handleEvent = eventHandler,
        render = body,
        destructor = destructor,
        mergeStates = mergeStates
      )

  end new
end statefulWidget
