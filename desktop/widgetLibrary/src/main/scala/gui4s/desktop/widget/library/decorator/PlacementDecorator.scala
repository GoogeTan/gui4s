package gui4s.desktop.widget.library
package decorator

import catnip.syntax.function.andThen
import cats.syntax.all.*
import cats.{Functor, ~>}

def placementDecorator[
  Update[_] : Functor,
  OldPlace[_] : Functor,
  NewPlace[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  placementShift : OldPlace ~> NewPlace
)(
  original : OldPlace[Widget[Update, OldPlace, Draw, RecompositionReaction, HandleableEvent]]
) : NewPlace[Widget[Update, NewPlace, Draw, RecompositionReaction, HandleableEvent]] =
  placementShift(
    original,
  ).map(
    placedWidget =>
      Widget.ValueWrapper[
        Widget[Update, OldPlace, Draw, RecompositionReaction, HandleableEvent],
        Update,
        NewPlace,
        Draw,
        RecompositionReaction,
        HandleableEvent
      ](
        valueToDecorate = placedWidget,
        valueAsFree = placed => placementShift(placed.asFree),
        valueIsDrawable = widgetIsDrawable,
        valueHandlesEvent =
          widgetHandlesEvent[Update, OldPlace, Draw, RecompositionReaction, HandleableEvent]
            .andThen(_.map(placementShift[Widget[Update, OldPlace, Draw, RecompositionReaction, HandleableEvent]])),
        valueMergesWithOldState =
          widgetMergesWithOldState[Update, OldPlace, Draw, RecompositionReaction, HandleableEvent]
            .andThen(placementShift(_)),
        valueReactsOnRecomposition = widgetReactsOnRecomposition,
        valueHasInnerState = widgetHasInnerStates
      )
  )
end placementDecorator
