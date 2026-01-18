package gui4s.core.widget
package merge

import gui4s.core.widget.free.AsFreeF

//TODO дать более общее имя, соответсвующее сути дела
def statefulMergesWithOldStates[
  Place[_],
  WidgetState,
  SavedState,
  RecompositionReaction
](
   typeCheckState: [T] => (Any, Path, SavedState => Place[T]) => Place[T],
   mergeStates : (Path, SavedState, WidgetState) => Place[WidgetState],
   stateName : WidgetState => String,
    widgetStateAsFree : AsFreeF[WidgetState, Place]
) : MergesWithOldStates[
  WidgetState,
  RecompositionReaction,
  Place[
    WidgetState
  ],
] =
  (self, path, innerStates) =>
    innerStates.get(stateName(self)).fold(
      widgetStateAsFree(self)
    )(oldState =>
      typeCheckState(
        oldState.state,//TODO проверить, что происходит, если раньше состояния не было.
        path,
        mergeStates(path, _, self)
      )
    )
end statefulMergesWithOldStates
