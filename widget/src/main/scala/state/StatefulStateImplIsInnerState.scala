package me.katze.gui4s.widget
package state

def statefulHasInnerStates[
  PlacedWidget,
  State : Equiv as EQ,
  Draw,
  EventHandler,
  RecompositionReaction
](
  widgetHasInnerStates : HasInnerStates[PlacedWidget, RecompositionReaction]
): HasInnerStates[
  Stateful[PlacedWidget, StatefulState[State, Draw, EventHandler, State => RecompositionReaction]],
  RecompositionReaction
] =
  self =>
    val state = self.state
    Map(
      self.name -> StateTree((state.initialState, state.currentState), state.destructor(state.currentState), widgetHasInnerStates(self.child))
    )
end statefulHasInnerStates

