package gui4s.core.widget
package merge

import gui4s.core.widget.free.AsFreeF

//TODO дать более общее имя, соответсвующее сути дела
//TODO На самом деле, тут можно обобщить. Option * Place везде встречаются вместе неразрывно.
def statefulMergesWithOldStates[
  Place[_],
  WidgetState,
  SavedState,
  RecompositionReaction
](
   typeCheckState: [T] => (Any, SavedState => Option[Place[T]]) =>  Option[Place[T]],
   mergeStates : (SavedState, WidgetState) => Option[Place[WidgetState]],
   stateName : WidgetState => String,
   widgetStateAsFree : AsFreeF[WidgetState, Place]
) : MergesWithOldStates[
  WidgetState,
  RecompositionReaction,
  Option[Place[WidgetState]],
] =
  (self, innerStates) =>
    innerStates.get(stateName(self)).fold(
      None
    )(oldState =>
      typeCheckState(
        oldState.state,//TODO проверить, что происходит, если раньше состояния не было.
        mergeStates(_, self)
      )
    )
end statefulMergesWithOldStates
