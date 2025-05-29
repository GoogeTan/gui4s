package me.katze.gui4s.example
package api.widget

import api.*

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monoid}
import me.katze.gui4s.widget.draw.{statefulIsDrawable, statefulStateDrawsIntoWidget}
import me.katze.gui4s.widget.free.statefulAsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, statefulHandlesEvent, statefulStateHandlesEvents}
import me.katze.gui4s.widget.merge.{Mergable, mergeStatefulWithOldStates, mergeWithOldStatesStateful}
import me.katze.gui4s.widget.recomposition.{hasNoReactionOnRecomposition, statefulReactsOnRecomposition}
import me.katze.gui4s.widget.state.{statefulHasInnerStates, statefulStateIsState}
import me.katze.gui4s.widget.{CatchEvents, Path, Stateful, StatefulState}

def skijaStateful[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Place[+_] : Functor,
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
  TaskSupervisor,
  EventReaction,
  State,
  Event,
  ChildEvent
](
  widgetsAreMergeable : Mergable[Place, SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]],
  taskSupervisor: TaskSupervisor,
  runEventReaction : (EventReaction, Path, TaskSupervisor) => Update[State, Event],
  typeCheckState : Any => Place[(State, State)],
)(
  name : String,
  initialState : State,
  handleEvent : (State, List[ChildEvent]) => EventReaction,
  render : State => Place[SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]],
  destructor : State => RecompositionReaction,
) : Place[
  SkijaWidget_[
    [Value] =>> Update[Value, Event],
    Place,
    Draw,
    RecompositionReaction,
    HandlableEvent
  ]
] =
  render(initialState).map(initialChild =>
    type Widget[E] = SkijaWidget_[
      [Value] =>> Update[Value, E],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ]
    type StState = StatefulState[
      State,
      State => Place[Widget[ChildEvent]],
      (State, Path, List[ChildEvent]) => Update[State, Event],
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
            runEventReaction(handleEvent(state, events), path, taskSupervisor),
        destructor = destructor
      ),
      child = initialChild
    )

    SkijaWidget[
      Stateful[Widget[ChildEvent], StState],
      [Value] =>> Update[Value, Event],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ](
      valueToDecorate = stateful,
      valueAsFree = statefulAsFree(skijaWidgetAsFree),
      valueIsDrawable = statefulIsDrawable(skijaWidgetIsDrawable),
      valueHandlesEvent = skijaStatefulHandlesEvent(widgetsAreMergeable),
      valueMergesWithOldState = mergeWithOldStatesStateful(mergeStatefulWithOldStates(typeCheckState)),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        skijaWidgetReactsOnRecomposition[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent],
      ),
      valueHasInnerState = statefulHasInnerStates(statefulStateIsState)
    )
  )
end skijaStateful

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
  widgetsAreMergeable : Mergable[Place, SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]],
): HandlesEvent[
  Stateful[
    SkijaWidget_[
      [Value] =>> Update[Value, ChildEvent],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ],
    StatefulState[
      State,
      State => Place[
        SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]
      ],
      (State, Path, List[ChildEvent]) => Update[State, Event],
      State => RecompositionReaction
    ]
  ],
  HandlableEvent,
  Update[
    Place[
      Stateful[
        SkijaWidget_[
          [Value] =>> Update[Value, ChildEvent],
          Place,
          Draw,
          RecompositionReaction,
          HandlableEvent
        ],
        StatefulState[
          State,
          State => Place[
            SkijaWidget_[[Value] =>> Update[Value, ChildEvent],
              Place, Draw, RecompositionReaction, HandlableEvent
            ]
          ],
          (State, Path, List[ChildEvent]) => Update[State, Event],
          State => RecompositionReaction]
      ]
    ],
    Event
  ]
] = statefulHandlesEvent(
  stateHandlesEvents = statefulStateHandlesEvents[
    [Value] =>> Update[Value, Event],
    State,
    State =>
      Place[
        SkijaWidget_[
          [Value] =>> Update[Value, ChildEvent], Place, Draw,
          RecompositionReaction, HandlableEvent]
      ],
    ChildEvent,
    State => RecompositionReaction
  ],
  drawStateIntoWidget = statefulStateDrawsIntoWidget,
  childHandlesEvents = skijaWidgetHandlesEvent[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent],
  widgetsAreMergable = widgetsAreMergeable,
  widgetAsFree = skijaWidgetAsFree
)
