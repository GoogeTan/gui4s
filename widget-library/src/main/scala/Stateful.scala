package me.katze.gui4s.widget.library

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monoid}
import me.katze.gui4s.widget.draw.{statefulIsDrawable, statefulStateDrawsIntoWidget}
import me.katze.gui4s.widget.free.statefulAsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, statefulHandlesEvent, statefulStateHandlesEvents}
import me.katze.gui4s.widget.merge.{Mergable, mergeStatefulWithOldStates, mergeWithOldStatesStateful}
import me.katze.gui4s.widget.recomposition.statefulReactsOnRecomposition
import me.katze.gui4s.widget.state.{statefulHasInnerStates, statefulStateIsState}
import me.katze.gui4s.widget.{CatchEvents, Path, Stateful, StatefulState}

import scala.language.experimental.namedTypeArguments

def skijaStateful[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Place[+_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  EventReaction,
  State,
  Event,
  ChildEvent
](
  widgetsAreMergeable : Mergable[Place, SkijaWidget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
  runEventReaction : (EventReaction, Path) => Update[Event, State],
  typeCheckState : Any => Place[(State, State)],
)(
  name : String,
  initialState : State,
  handleEvent : (State, List[ChildEvent]) => EventReaction,
  render : State => Place[SkijaWidget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
  destructor : State => RecompositionReaction,
) : Place[
  SkijaWidget_[
    Update[Event, *],
    Place,
    Draw,
    RecompositionReaction,
    HandlableEvent
  ]
] =
  render(initialState).map(initialChild =>
    type Widget[E] = SkijaWidget_[
      Update[E, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ]
    type StState = StatefulState[
      State,
      State => Place[Widget[ChildEvent]],
      (State, Path, List[ChildEvent]) => Update[Event, State],
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
          (state : State, path : Path, events : List[ChildEvent]) =>
            runEventReaction(handleEvent(state, events), path),
        destructor = destructor
      ),
      child = initialChild
    )

    SkijaWidget[
      T = Stateful[Widget[ChildEvent], StState],
      Update = Update[Event, *],
      Place = Place
    ](
      valueToDecorate = stateful,
      valueAsFree = statefulAsFree(skijaWidgetAsFree),
      valueIsDrawable = statefulIsDrawable(skijaWidgetIsDrawable),
      valueHandlesEvent = skijaStatefulHandlesEvent(widgetsAreMergeable),
      valueMergesWithOldState = mergeWithOldStatesStateful(mergeStatefulWithOldStates(typeCheckState)),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        skijaWidgetReactsOnRecomposition[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent],
      ),
      valueHasInnerState = statefulHasInnerStates(statefulStateIsState)
    )
  )
end skijaStateful

type HandlesEventPlace[Place[_], T, HandlableEvent] = HandlesEvent[T, HandlableEvent, Place[T]]

def skijaStatefulHandlesEvent[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Place[+_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  State,
  Event,
  ChildEvent
](
  widgetsAreMergeable : Mergable[Place, SkijaWidget_[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent]],
): HandlesEventPlace[
  [T] =>> Update[Event, Place[T]],
  Stateful[
    SkijaWidget_[
      Update[ChildEvent, *],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ],
    StatefulState[
      State,
      State => Place[
        SkijaWidget_[
          Update[ChildEvent, *],
          Place, Draw, RecompositionReaction, HandlableEvent
        ]
      ],
      HandlesEvent[State, List[ChildEvent], Update[Event, State]],
      State => RecompositionReaction]
  ],
  HandlableEvent,
] = statefulHandlesEvent(
  stateHandlesEvents = statefulStateHandlesEvents[Update = Update[Event, *]],
  drawStateIntoWidget = statefulStateDrawsIntoWidget,
  childHandlesEvents = skijaWidgetHandlesEvent[Update[ChildEvent, *], Place, Draw, RecompositionReaction, HandlableEvent],
  widgetsAreMergable = widgetsAreMergeable,
  widgetAsFree = skijaWidgetAsFree
)

