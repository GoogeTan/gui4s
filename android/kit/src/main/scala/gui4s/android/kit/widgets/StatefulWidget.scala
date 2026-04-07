package gui4s.android.kit.widgets

import gui4s.core.layout.Sized.given
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.*
import gui4s.core.widget.StatefulState
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given
import gui4s.desktop.widget.library
import gui4s.desktop.widget.library.*

import scala.reflect.Typeable


import cats.effect.IO
import gui4s.core.layout.Sized.given
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.handle.HandlesEvent
import gui4s.core.widget.library.*
import gui4s.core.widget.StatefulState
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given
import gui4s.desktop.widget.library
import gui4s.desktop.widget.library.*
import cats.data.NonEmptyList

import scala.reflect.Typeable


def statefulWidget: StatefulWidget[
  AndroidWidget,
  Update,
  * => RecompositionReaction,
  MergeStates[Place, *]
] =
  new StatefulWidget[
    AndroidWidget,
    Update,
    * => RecompositionReaction,
    MergeStates[Place, *]
  ]:
    override def apply[State: {Equiv, Typeable}, Event, ChildEvent](
                                                                      name: String,
                                                                      initialState: State,
                                                                      eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event]],
                                                                      body: State => AndroidWidget[ChildEvent]
                                                                    ): AndroidWidget[Event] =
      apply(name, initialState, eventHandler, body, _ => RecompositionReaction.empty)
    end apply

    @SuppressWarnings(Array("org.wartremover.warts.ToString"))
    override def apply[State: {Equiv as EQ, Typeable}, Event, ChildEvent](
                                                                      name: String,
                                                                      initialState: State,
                                                                      eventHandler: HandlesEventF[State, List[ChildEvent], UpdateC[Event]],
                                                                      body: State => AndroidWidget[ChildEvent],
                                                                      destructor: State => RecompositionReaction
                                                                    ): AndroidWidget[Event] =
      apply[State, Event, ChildEvent](
        name = name,
        initialState = initialState,
        eventHandler =
          (originalState, path, events) =>
            eventHandler(originalState, path, events).map(
              resultingState =>
                if EQ.equiv(originalState, resultingState) then
                  None
                else
                  Some(resultingState)
            ),
        body = body,
        destructor = destructor,
        mergeStates = [T] => (oldState : StatefulState[State], newState : StatefulState[State], consumer : StatefulState[State] => Place[T]) =>
          if EQ.equiv(oldState.initialState, newState.initialState) then
             Some(consumer(oldState))  // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
          else
             None
      )
    end apply

    override def apply[State, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, List[ChildEvent], Update[Event, *] * Option],
                                                            body: State => AndroidWidget[ChildEvent],
                                                            destructor: State => RecompositionReaction,
                                                            mergeStates: MergeStates[Place, State]
                                                          ): AndroidWidget[Event] =
      library.stateful[
        UpdateC[Event],
        UpdateC[ChildEvent],
        Place,
        Draw,
        RecompositionReaction,
        State,
        ChildEvent
      ](
        widgetsAreMergeable = new widgetsCanUpdateStateFromTheOldOnes(
          widgetMergesWithOldState[UpdateC[ChildEvent], Place, Draw, RecompositionReaction],
          widgetHasInnerStates[UpdateC[ChildEvent], Place, Draw, RecompositionReaction]
        ),
        typeCheckState = Place.typecheck[StatefulState[State]](
          (valueToTypeCheck, path) =>  new Exception("Error in stateful typechecking at " + path.toString + " with value [" + valueToTypeCheck.toString + "]")
        ),
        liftUpdate = Update.catchEvents[ChildEvent, Event],
        addNameToPath = Place.addNameToPath,
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
