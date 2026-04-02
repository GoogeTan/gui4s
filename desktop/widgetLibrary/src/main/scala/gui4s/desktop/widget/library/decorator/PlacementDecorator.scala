package gui4s.desktop.widget.library
package decorator

import catnip.syntax.function.andThen
import cats.Functor
import cats.syntax.all._

def placementDecorator[
  Update[_] : Functor,
  OldPlace[_] : Functor,
  NewPlace[_] : Functor,
  Draw,
  RecompositionReaction
](
  placementShift : OldPlace[Widget[Update, OldPlace, Draw, RecompositionReaction]] =>
    NewPlace[Widget[Update, OldPlace, Draw, RecompositionReaction]],
  original : OldPlace[Widget[Update, OldPlace, Draw, RecompositionReaction]]
) : NewPlace[Widget[Update, NewPlace, Draw, RecompositionReaction]] =
  placementShift(
    original,
  ).map(
    placedWidget =>
      Widget.ValueWrapper[
        Widget[Update, OldPlace, Draw, RecompositionReaction],
        Update,
        NewPlace,
        Draw,
        RecompositionReaction
      ](
        valueToDecorate = placedWidget,
        valueAsFree = placed => placementShift(placed.asFree),
        valueIsDrawable = widgetIsDrawable,
        valueHandlesEvent =
          widgetHandlesEvent[Update, OldPlace, Draw, RecompositionReaction]
            .andThen(_.map(_.map(placementShift))),
        valueMergesWithOldState =
          widgetMergesWithOldState[Update, OldPlace, Draw, RecompositionReaction]
            .andThen(_.map(placementShift(_))),
        valueReactsOnRecomposition = widgetReactsOnRecomposition,
        valueHasInnerState = widgetHasInnerStates
      )
  )
end placementDecorator
