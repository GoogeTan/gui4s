package me.katze.gui4s.example
package api.widget

import api.{SkijaWidget, SkijaWidget_, skijaWidgetAsFree, skijaWidgetHandlesEvent, skijaWidgetIsDrawable, skijaWidgetReactsOnRecomposition}

import catnip.BiMonad
import cats.{Functor, Monoid}
import cats.syntax.all.*
import catnip.syntax.all.{*, given}
import me.katze.gui4s.widget.draw.{statefulIsDrawable, statefulStateDrawsIntoWidget}
import me.katze.gui4s.widget.free.statefulAsFree
import me.katze.gui4s.widget.handle.{HandlesEvent, statefulHandlesEvent, statefulStateHandlesEvents}
import me.katze.gui4s.widget.merge.{Mergable, mergeStatefulWithOldStates, mergeWithOldStatesStateful}
import me.katze.gui4s.widget.recomposition.{hasNoReactionOnRecomposition, statefulReactsOnRecomposition}
import me.katze.gui4s.widget.state.{statefulHasInnerStates, statefulStateIsState}
import me.katze.gui4s.widget.{CatchEvents, EventReaction, Path, Stateful, StatefulState}

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
  name : String,
  initialState : State,
  handleEvent : (State, List[ChildEvent]) => EventReaction,
  render : State => Place[SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]],
  destructor : State => RecompositionReaction,
  taskSupervisor: TaskSupervisor,
  runEventReaction : (EventReaction, Path, TaskSupervisor) => Update[State, Event],
  typeCheckState : Any => Place[(State, State)]
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
      TaskSupervisor,
      State => Place[Widget[ChildEvent]],
      (State, List[ChildEvent]) => EventReaction,
      State => RecompositionReaction
    ]
    SkijaWidget[
      Stateful[Widget[ChildEvent], StState],
      [Value] =>> Update[Value, Event],
      Place,
      Draw,
      RecompositionReaction,
      HandlableEvent
    ](
      valueToDecorate = Stateful(
        name = name,
        state = StatefulState(
          name = name,
          initialState = initialState,
          currentState = initialState,
          taskSupervisor = taskSupervisor,
          draw = render,
          handleEvents = handleEvent,
          destructor = destructor
        ),
        child = initialChild
      ),
      valueAsFree = statefulAsFree(skijaWidgetAsFree),
      valueIsDrawable = statefulIsDrawable(skijaWidgetIsDrawable),
      valueHandlesEvent = skijaStatefulHandlesEvent(widgetsAreMergeable, runEventReaction),
      valueMergesWithOldState = mergeWithOldStatesStateful(
        mergeStatefulWithOldStates(typeCheckState)      
      ),
      valueReactsOnRecomposition = statefulReactsOnRecomposition(
        widgetReactsOnRecomposition = skijaWidgetReactsOnRecomposition[
          [Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent
        ], 
        stateReactsOnRecomposition = hasNoReactionOnRecomposition(M.empty),
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
  TaskSupervisor,
  EventReaction,
  State,
  Event,
  ChildEvent
](
  widgetsAreMergeable : Mergable[Place, SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]],
  runEventReaction : (EventReaction, Path, TaskSupervisor) => Update[State, Event],
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
      TaskSupervisor, 
      State => Place[
        SkijaWidget_[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent]
      ], 
      (State, List[ChildEvent]) => EventReaction, 
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
          TaskSupervisor, 
          State => Place[
            SkijaWidget_[[Value] =>> Update[Value, ChildEvent], 
              Place, Draw, RecompositionReaction, HandlableEvent
            ]
          ], 
          (State, List[ChildEvent]) => EventReaction, 
          State => RecompositionReaction]
      ]
    ],
    Event
  ]
] = statefulHandlesEvent(
  stateHandlesEvents = statefulStateHandlesEvents[
    [Value] =>> Update[Value, Event],
    State,
    TaskSupervisor,
    State =>
      Place[
        SkijaWidget_[
          [Value] =>> Update[Value, ChildEvent], Place, Draw,
          RecompositionReaction, HandlableEvent]
      ],
    ChildEvent,
    EventReaction,
    State => RecompositionReaction
  ](runEventReaction),
  drawStateIntoWidget = statefulStateDrawsIntoWidget,
  childHandlesEvents = skijaWidgetHandlesEvent[[Value] =>> Update[Value, ChildEvent], Place, Draw, RecompositionReaction, HandlableEvent],
  widgetsAreMergable = widgetsAreMergeable,
  widgetAsFree = skijaWidgetAsFree
)
  