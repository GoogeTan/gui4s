package gui4s.desktop.widget.library

import catnip.syntax.all._
import cats.Comonad
import cats.Functor
import cats.Monad
import cats.syntax.all._

import gui4s.core.widget.merge.Mergable

def widgetsAreMergable[
  Update[_],
  PlacementEffect[_] : Monad as OPM,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
] : Mergable[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]]] =
  type Place[Value] = PlacementEffect[Situated[Value]]
  given Functor[Place] = nestedFunctorsAreFunctors[PlacementEffect, Situated]

  (path, oldWidget, newWidget) =>
    Monad[PlacementEffect].flatMap2(
      oldWidget, newWidget
    )(
      (situatedOld, nextWidgetToMerge) =>
        widgetMergesWithOldState[Update, Place, Draw, RecompositionReaction, HandleableEvent](
          nextWidgetToMerge.extract,
          path,
          widgetHasInnerStates(situatedOld.extract)
        )
    )
end widgetsAreMergable