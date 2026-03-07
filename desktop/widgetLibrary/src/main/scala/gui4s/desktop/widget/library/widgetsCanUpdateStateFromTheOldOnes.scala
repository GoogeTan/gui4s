package gui4s.desktop.widget.library

import catnip.syntax.all.*
import cats.*
import cats.syntax.all.*
import gui4s.core.widget.Path
import gui4s.core.widget.merge.{MergesWithOldStatesF, UpdateWidgetStateFromTheOldOne}
import gui4s.core.widget.state.HasInnerStates

class widgetsCanUpdateStateFromTheOldOnes[
  Widget,
  PlacementEffect[_] : Monad as PEFM,
  Situated[_] : Comonad,
  RecompositionReaction,
](
  merge : MergesWithOldStatesF[Widget, RecompositionReaction, Option * PlacementEffect * Situated],
  innerStates : HasInnerStates[Widget, RecompositionReaction]
) extends UpdateWidgetStateFromTheOldOne[PlacementEffect * Situated, Widget]:
  override def mergeUpdatedAndRerenderedWidgets(
    pathToWidget: Path,
    oldWidget: PlacementEffect[Situated[Widget]],
    newWidget: PlacementEffect[Situated[Widget]]
  ): PlacementEffect[Situated[Widget]] =
    PEFM.flatMap2(oldWidget, newWidget)(
      (situatedOld, situatedNew) =>
        merge(
          situatedNew.extract,
          pathToWidget,
          innerStates(situatedOld.extract)
        ).getOrElse(situatedNew.pure)
    )
  end mergeUpdatedAndRerenderedWidgets

  override def mergeOldAndRerenderedWidgets(
    pathToWidget: Path,
    oldWidget: Widget,
    newWidget: PlacementEffect[Situated[Widget]]
  ): PlacementEffect[Situated[Widget]] =
    PEFM.flatMap(newWidget)(
      situatedNew =>
        merge(
          situatedNew.extract,
          pathToWidget,
          innerStates(oldWidget)
        ).getOrElse(situatedNew.pure)
    )
  end mergeOldAndRerenderedWidgets
end widgetsCanUpdateStateFromTheOldOnes