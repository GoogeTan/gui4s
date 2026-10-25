package gui4s.core.widget
package state

def statefulHasInnerStates[
  PlacedWidget,
  State,
  Draw,
  EventHandler,
  RecompositionReaction
](
  widgetHasInnerStates : HasInnerStates[PlacedWidget, RecompositionReaction],
  noRecompositionReaction : RecompositionReaction
): HasInnerStates[
  Stateful[PlacedWidget, StatefulBehaviour[State, Draw, EventHandler]],
  RecompositionReaction
] =
  self =>
    val behaviour = self.stateBehaviour
    Map(
      self.name -> StateTree(behaviour.state, noRecompositionReaction, widgetHasInnerStates(self.child))
    )
end statefulHasInnerStates

