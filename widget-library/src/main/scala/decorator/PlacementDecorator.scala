package me.katze.gui4s.widget.library
package decorator

import decorator.Decorator

import cats.syntax.all.*
import cats.{Functor, Monad}

def placementDecorator[
  Update[_] : Functor,
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  placementShift : [T] => Place[T] => Place[T]
) : Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]] =
  original =>
    placementShift(
      original,
    ).map(
      placedWidget =>
        Widget.ValueWrapper[
          Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent],
          Update,
          Place,
          Draw,
          RecompositionReaction,
          HandleableEvent
        ](
          valueToDecorate = placedWidget,
          valueAsFree = placed => placementShift(placed.asFree),
          valueIsDrawable = widgetIsDrawable,
          valueHandlesEvent = widgetHandlesEvent,
          valueMergesWithOldState = widgetMergesWithOldState,
          valueReactsOnRecomposition = widgetReactsOnRecomposition,
          valueHasInnerState = widgetHasInnerStates
        )
    )
end placementDecorator
