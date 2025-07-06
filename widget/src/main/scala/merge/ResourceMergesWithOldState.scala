package me.katze.gui4s.widget
package merge

import free.AsFreeF

import cats.Functor
import cats.syntax.all.*

def resourceMergesWithOldState[
  Widget,
  Value,
  RecompositionAction,
  Place[_] : Functor
](
  widgetMergesWithOldState : MergesWithOldStatesF[Widget, RecompositionAction, Place],
  resourceAsFree : AsFreeF[Resource[Widget, Value], Place],
  typeCheckState: [T] => (Any, Path, Value => Place[T]) => Place[T],
) : MergesWithOldStatesF[
  Resource[
    Widget,
    Value,
  ],
  RecompositionAction,
  Place
] =
  (self, pathToParent, oldStates) =>
    val pathToSelf = pathToParent.appendLast(self.name)
    oldStates.get(self.name) match
      case Some(StateTree(oldState, _, childrenStates)) =>
        typeCheckState(
          oldState,
          pathToSelf,
          value =>
            widgetMergesWithOldState(self.widget, pathToSelf, childrenStates).map(
              newWidget =>
                self.copy(
                  widget = newWidget,
                  value = value
                )
            )
        )
      case None =>
        resourceAsFree(self)
end resourceMergesWithOldState
