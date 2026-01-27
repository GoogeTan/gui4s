package gui4s.desktop.widget.library
package decorator

import catnip.syntax.function.andThen
import cats.Functor
import cats.syntax.all._
import cats.~>

def placementDecorator[
  Update[_] : Functor,
  OldPlace[_] : Functor,
  NewPlace[_] : Functor,
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
](
  placementShift : OldPlace[Widget[Update, OldPlace, Draw, RecompositionReaction, EnvironmentalEvent]] =>
    NewPlace[Widget[Update, OldPlace, Draw, RecompositionReaction, EnvironmentalEvent]]
)(
  original : OldPlace[Widget[Update, OldPlace, Draw, RecompositionReaction, EnvironmentalEvent]]
) : NewPlace[Widget[Update, NewPlace, Draw, RecompositionReaction, EnvironmentalEvent]] =
  placementShift(
    original,
  ).map(
    placedWidget =>
      Widget.ValueWrapper[
        Widget[Update, OldPlace, Draw, RecompositionReaction, EnvironmentalEvent],
        Update,
        NewPlace,
        Draw,
        RecompositionReaction,
        EnvironmentalEvent
      ](
        valueToDecorate = placedWidget,
        valueAsFree = placed => placementShift(placed.asFree),
        valueIsDrawable = widgetIsDrawable,
        valueHandlesEvent =
          widgetHandlesEvent[Update, OldPlace, Draw, RecompositionReaction, EnvironmentalEvent]
            .andThen(_.map(placementShift)),
        valueMergesWithOldState =
          widgetMergesWithOldState[Update, OldPlace, Draw, RecompositionReaction, EnvironmentalEvent]
            .andThen(placementShift(_)),
        valueReactsOnRecomposition = widgetReactsOnRecomposition,
        valueHasInnerState = widgetHasInnerStates
      )
  )
end placementDecorator
