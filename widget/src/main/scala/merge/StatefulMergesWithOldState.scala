package me.katze.gui4s.widget
package merge

import free.AsFreeF

import cats.Functor

def statefulMergesWithOldStates[
  Place[_] : Functor,
  State: Equiv as EQ,
  Widget,
  Draw,
  EventHandler,
  RecompositionReaction
](
   typeCheckState: [T] => (Any, (State, State) => Place[T]) => Place[T],
   stateAsFree : AsFreeF[
      Stateful[
        Widget,
        StatefulState[State, Draw, EventHandler, State => RecompositionReaction],
      ],
      Place
   ]
) : MergesWithOldStates[
  Stateful[
    Widget,
    StatefulState[State, Draw, EventHandler, State => RecompositionReaction],
  ],
  RecompositionReaction,
  Place[
    Stateful[
      Widget,
      StatefulState[State, Draw, EventHandler, State => RecompositionReaction],
    ]
  ],
] =
  (self, _, innerStates) =>
    typeCheckState(
      innerStates(self.name),
      (oldInitialState, oldState) =>
        stateAsFree(
          if EQ.equiv(oldInitialState, self.state.initialState) then
            self.copy(
              state =
                self.state.copy(currentState = oldState) // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
            )
          else
            self
        )
    )
end statefulMergesWithOldStates
