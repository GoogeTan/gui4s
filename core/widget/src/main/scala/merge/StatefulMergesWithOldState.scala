package gui4s.core.widget
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
    typeCheckState: [T] => (Any, Path, StatefulState[State] => Place[T]) => Place[T],
    stateAsFree : AsFreeF[
      Stateful[
        Widget,
        StatefulBehaviour[State, Draw, EventHandler, State => RecompositionReaction],
      ],
      Place
    ]
) : MergesWithOldStates[
  Stateful[
    Widget,
    StatefulBehaviour[State, Draw, EventHandler, State => RecompositionReaction],
  ],
  RecompositionReaction,
  Place[
    Stateful[
      Widget,
      StatefulBehaviour[State, Draw, EventHandler, State => RecompositionReaction],
    ]
  ],
] =
  (self, path, innerStates) =>
    typeCheckState(
      innerStates(self.name).state,
      path,
      oldState =>
        stateAsFree(
          if EQ.equiv(oldState.initialState, self.stateBehaviour.state.initialState) then
            self.copy(stateBehaviour = self.stateBehaviour.withNewState(oldState.currentState)) // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
          else
            self
        )
    )
end statefulMergesWithOldStates
