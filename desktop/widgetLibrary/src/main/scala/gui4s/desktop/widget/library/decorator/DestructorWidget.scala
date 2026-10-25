package gui4s.desktop.widget.library.decorator

import cats.Functor
import cats.syntax.all.*
import gui4s.core.widget.StateTree
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.widget.library.*

def destructorWidget[
  Update[_] : Functor,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction,
](
  name : String,
  destructor : RecompositionReaction
) : Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction]]] =
  _.map(
    original =>
      Widget.ValueWrapper(
        valueToDecorate = original,
        valueAsFree = widgetAsFree,
        valueIsDrawable = widgetIsDrawable,
        valueHandlesEvent = widgetHandlesEvent,
        valueMergesWithOldState = widgetMergesWithOldState,
        valueReactsOnRecomposition = widgetReactsOnRecomposition,
        valueHasInnerState = or =>
          widgetHasInnerStates(or) ++ Map(name -> StateTree((), destructor, Map()))
      )
  )
end destructorWidget
