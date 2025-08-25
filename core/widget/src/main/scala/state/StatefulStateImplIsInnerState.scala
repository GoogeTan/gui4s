package gui4s.core.widget
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
  Stateful[PlacedWidget, StatefulBehaviour[State, Draw, EventHandler, State => RecompositionReaction]],
  RecompositionReaction
] =
  self =>
    val behaviour = self.stateBehaviour
    Map(
      self.name -> StateTree(behaviour.state, behaviour.destructor(behaviour.state.currentState), widgetHasInnerStates(self.child))
    )
end statefulHasInnerStates

