package gui4s.desktop.widget.library.decorator

import cats.*
import gui4s.desktop.widget.library.*
import gui4s.core.layout.rowcolumn.*
import gui4s.core.widget.library.LinearContainer
import gui4s.core.widget.library.decorator.Decorator

def fixedSizeWidget[
  Update[_] : Functor,
  OuterPlace[_],
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  BoundUnit,
  MeasurementUnit,
  Axis,
](
   withPreferredSize : Place ~> Place,
   linearContainer: LinearContainer[
    Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]],
    OuterPlace,
    Id,
    BoundUnit,
    MeasurementUnit,
    Axis
  ],
   mainAxis : Axis,
   mainAxisStrategy : OneElementPlacementStrategy[OuterPlace, BoundUnit, MeasurementUnit],
   crossAxisStrategy : OneElementPlacementStrategy[OuterPlace, BoundUnit, MeasurementUnit],
) : Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]] =
  originalWidget =>
    placementDecorator(
      withPreferredSize
    )(
      linearContainer(
        originalWidget,
        mainAxis,
        mainAxisStrategy,
        crossAxisStrategy
      )
    )
end fixedSizeWidget