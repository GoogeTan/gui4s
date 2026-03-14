package gui4s.desktop.widget.library

import catnip.syntax.all.{*, given}
import cats.*
import gui4s.core.geometry.*
import gui4s.core.layout.{Measured, OneElementPlacementStrategy, PlacementStrategy, Sized, SizedC}
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.widget.library.decorator.{FreeWidgetWithSituated, WidgetWithSituated}

//TODO document me
def layersWidget[
  Update[_] : Monad,
  PlacementEffect[_] : Monad as OPA,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  EnvironmentalEvent,
  MeasurementUnit : Numeric as MUN,
  BoundUnit,
](
  container : ContainerWidget[
    FreeWidgetWithSituated[Update, PlacementEffect, SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent],
    List[
      FreeWidgetWithSituated[Update, PlacementEffect, SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent],
    ],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[MeasurementUnit, BoundUnit, WidgetWithSituated[Update, PlacementEffect, SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent]]],
      Rect[MeasurementUnit],
      Rect[BoundUnit],
      List,
       Measured[MeasurementUnit, BoundUnit,
         (
           WidgetWithSituated[Update, PlacementEffect, SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent],
           Point3d[MeasurementUnit]
         )
       ]
    ]
  ],
  withBounds : Rect[MeasurementUnit] => PlacementEffect ~> PlacementEffect
)(
   background : List[PlacementEffect[Sized[MeasurementUnit, Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent]]]],
   foreground : List[PlacementEffect[Sized[MeasurementUnit, Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent]]]],
   decorationsPlacementStrategy : PlacementStrategy[PlacementEffect, Rect[MeasurementUnit], Rect[MeasurementUnit], Rect[MeasurementUnit], List, Point2d[MeasurementUnit]],
) : Decorator[
  PlacementEffect[Sized[MeasurementUnit, Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, EnvironmentalEvent]]]
] =
  gui4s.core.widget.library.layersWidget(container, withBounds)(
    background, 
    foreground,
    decorationsPlacementStrategy,
    PlacementStrategy.Zip(
      OneElementPlacementStrategy.Begin,
      OneElementPlacementStrategy.Begin,
    ) 
  )
end layersWidget
