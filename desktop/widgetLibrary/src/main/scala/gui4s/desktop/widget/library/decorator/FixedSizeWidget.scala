package gui4s.desktop.widget.library.decorator

import cats._

import gui4s.core.layout._
import gui4s.core.widget.library.LinearContainer
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.widget.library._

def fixedSizeWidget[
  Update[_] : Functor,
  PlacementEffect[_],
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  EnvironmentalEvent,
  BoundUnit,
  MeasurementUnit,
  Axis,
](
   withPreferredSize : Place ~> Place,
   linearContainer: LinearContainer[
    Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]],
    PlacementEffect,
    Id,
    BoundUnit,
    MeasurementUnit,
    Axis
  ],
   mainAxis : Axis,
   mainAxisStrategy : OneElementPlacementStrategy[PlacementEffect, MeasurementUnit, MeasurementUnit, BoundUnit, MeasurementUnit],
   crossAxisStrategy : OneElementPlacementStrategy[PlacementEffect, MeasurementUnit, MeasurementUnit, BoundUnit, MeasurementUnit],
) : Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction, EnvironmentalEvent]]] =
  originalWidget =>
    placementDecorator[
      Update,
      Place,
      Place,
      Draw,
      RecompositionReaction,
      EnvironmentalEvent
    ](
      withPreferredSize(_)
    )(
      linearContainer(
        originalWidget,
        mainAxis,
        mainAxisStrategy,
        crossAxisStrategy
      )
    )
end fixedSizeWidget