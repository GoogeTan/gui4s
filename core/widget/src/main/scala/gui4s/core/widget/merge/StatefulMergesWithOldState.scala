package gui4s.core.widget
package merge

//TODO дать более общее имя, соответствующее сути дела
//TODO На самом деле, тут можно обобщить. Option * Place везде встречаются вместе неразрывно
def statefulMergesWithOldStates[
  Place[_],
  WidgetState,
  SavedState,
  RecompositionReaction
](
  mergeStates : (Any, WidgetState) => Option[Place[WidgetState]],
  stateName : WidgetState => String
) : MergesWithOldStates[
  WidgetState,
  RecompositionReaction,
  Option[Place[WidgetState]],
] =
  (self, innerStates) =>
    innerStates.get(stateName(self)).fold(
      None
    )(oldState =>
      mergeStates(oldState.state, self)//TODO проверить, что происходит, если раньше состояния не было.
    )
end statefulMergesWithOldStates
