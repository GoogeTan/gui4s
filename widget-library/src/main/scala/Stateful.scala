package me.katze.gui4s.widget.library

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.{Functor, Monoid}
import me.katze.gui4s.widget.draw.{statefulIsDrawable, statefulStateDrawsIntoWidget}
import me.katze.gui4s.widget.free.statefulAsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, statefulHandlesEvent, statefulStateHandlesEvents}
import me.katze.gui4s.widget.merge.{Mergable, statefulMergesWithOldStates}
import me.katze.gui4s.widget.recomposition.statefulReactsOnRecomposition
import me.katze.gui4s.widget.state.statefulHasInnerStates
import me.katze.gui4s.widget.{CatchEvents, Path, Stateful, StatefulState}

import scala.language.experimental.namedTypeArguments

def stateful[
  Update[_, _] : {BiMonad, CatchEvents},
  Place[_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  EventReaction,
  State,
  Event,
  ChildEvent
](
   widgetsAreMergeable : Mergable[Place, Widget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
   runEventReaction : (EventReaction, Path) => Update[Event, State],
   typeCheckState : [T] => (Any, Path, (State, State) => Place[T]) => Place[T],
)(
   name : String,
   initialState : State,
   handleEvent : (State, NonEmptyList[ChildEvent]) => EventReaction,
   render : State => Place[Widget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
   destructor : State => RecompositionReaction,
) : Place[
  Widget_[
    Update[Event, *],
    Place,
    Draw,
    RecompositionReaction,
    HandlableEvent
  ]
] =
  render(initialState).map(initialChild =>
    type Widget[E] = Widget_[
      Update[E, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ]
    type StState = StatefulState[
      State,
      State => Place[Widget[ChildEvent]],
      (State, Path, NonEmptyList[ChildEvent]) => Update[Event, State],
      State => RecompositionReaction
    ]
    val stateful = Stateful(
      name = name,
      state = StatefulState(
        name = name,
        initialState = initialState,
        currentState = initialState,
        draw = render,
        handleEvents =
          (state : State, path : Path, events : NonEmptyList[ChildEvent]) =>
            runEventReaction(handleEvent(state, events), path),
        destructor = destructor
      ),
      child = initialChild
    )
    val statefulAsFree_ = statefulAsFree[Place, Widget[ChildEvent], StState](widgetAsFree)
    Widget[
      T = Stateful[Widget[ChildEvent], StState],
      Update = Update[Event, *],
      Place = Place
    ](
      valueToDecorate = stateful,
      valueAsFree = statefulAsFree_, // TODO Rename me
      valueIsDrawable = statefulIsDrawable(widgetIsDrawable),
      valueHandlesEvent = statefulHandlesEvent_(widgetsAreMergeable),
      valueMergesWithOldState = statefulMergesWithOldStates(typeCheckState, statefulAsFree_),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        widgetReactsOnRecomposition[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent],
        M.empty
      ),
      valueHasInnerState = statefulHasInnerStates(widgetHasInnerStates)
    )
  )
end stateful

type HandlesEventPlace[Place[_], T, HandlableEvent] = HandlesEvent[T, HandlableEvent, Place[T]]

def statefulHandlesEvent_[
  Update[_, _] : {BiMonad, CatchEvents},
  Place[_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  State,
  Event,
  ChildEvent
](
   widgetsAreMergeable : Mergable[Place, Widget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
): HandlesEventPlace[
  [T] =>> Update[Event, Place[T]],
  Stateful[
    Widget_[
      Update[ChildEvent, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ],
    StatefulState[
      State,
      State => Place[
        Widget_[
          Update[ChildEvent, *],
          Place, Draw, RecompositionReaction, HandlableEvent
        ]
      ],
      HandlesEvent[State, NonEmptyList[ChildEvent], Update[Event, State]],
      State => RecompositionReaction]
  ],
  HandlableEvent,
] = statefulHandlesEvent(
  stateHandlesEvents = statefulStateHandlesEvents[Update = Update[Event, *]],
  drawStateIntoWidget = statefulStateDrawsIntoWidget,
  childHandlesEvents = widgetHandlesEvent[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent],
  widgetsAreMergable = widgetsAreMergeable,
  widgetAsFree = widgetAsFree
)

