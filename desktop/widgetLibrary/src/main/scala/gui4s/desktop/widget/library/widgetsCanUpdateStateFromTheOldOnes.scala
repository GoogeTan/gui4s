package gui4s.desktop.widget.library

import catnip.syntax.all.*
import cats.syntax.all.*
import cats.*

import gui4s.core.widget.merge.UpdateWidgetStateFromTheOldOne

def widgetsCanUpdateStateFromTheOldOnes[
  Update[_],
  PlacementEffect[_] : FlatMap as PEFM,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
] : UpdateWidgetStateFromTheOldOne[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, EnvironmentalEvent]]]] =
  (path, oldWidget, newWidget) =>
    PEFM.flatMap2(oldWidget, newWidget)(
      (situatedOld, situatedNew) =>
        widgetMergesWithOldState[Update, PlacementEffect * Situated, Draw, RecompositionReaction, EnvironmentalEvent](
          situatedNew.extract,
          path,
          widgetHasInnerStates(situatedOld.extract)
        )
    )
end widgetsCanUpdateStateFromTheOldOnes