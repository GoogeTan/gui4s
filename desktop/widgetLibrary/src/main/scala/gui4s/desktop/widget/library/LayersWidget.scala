package gui4s.desktop.widget.library

import catnip.syntax.all.{_, given}
import cats._

import gui4s.core.geometry._
import gui4s.core.layout.Measured
import gui4s.core.layout.OneElementPlacementStrategy
import gui4s.core.layout.PlacementStrategy
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.widget.library.decorator.FreeWidgetWithSituated
import gui4s.desktop.widget.library.decorator.WidgetWithSituated

//TODO document me
def layersWidget[
  Update[_] : Monad,
  PlacementEffect[_] : Monad as OPA,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  EnvironmentalEvent,
  MeasurementUnit : Numeric,
  BoundUnit,
](
  container : ContainerWidget[
    FreeWidgetWithSituated[Update, PlacementEffect, SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent],
    List[
      FreeWidgetWithSituated[Update, PlacementEffect, SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent],
    ],
    PlacementStrategy[
      PlacementEffect,
      PlacementEffect[Measured[Rect[MeasurementUnit], Rect[BoundUnit], WidgetWithSituated[Update, PlacementEffect, SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent]]],
      Rect[MeasurementUnit],
      Rect[BoundUnit],
      List,
       Measured[Rect[MeasurementUnit], Rect[BoundUnit],
         (
           WidgetWithSituated[Update, PlacementEffect, SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent],
           Point3d[MeasurementUnit]
         )
       ]
    ]
  ],
  withBounds : Rect[MeasurementUnit] => PlacementEffect ~> PlacementEffect
)(
   background : List[PlacementEffect[Sized[Rect[MeasurementUnit], Widget[Update, PlacementEffect * SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent]]]],
   foreground : List[PlacementEffect[Sized[Rect[MeasurementUnit], Widget[Update, PlacementEffect * SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent]]]],
   decorationsPlacementStrategy : PlacementStrategy[PlacementEffect, Rect[MeasurementUnit], Rect[MeasurementUnit], Rect[MeasurementUnit], List, Point2d[MeasurementUnit]],
) : Decorator[
  PlacementEffect[Sized[Rect[MeasurementUnit], Widget[Update, PlacementEffect * SizedC[Rect[MeasurementUnit]], Draw, RecompositionReaction, EnvironmentalEvent]]]
] =
  gui4s.core.widget.library.layersWidget(container, withBounds)(
    background, 
    foreground,
    decorationsPlacementStrategy,
    PlacementStrategy.Zip(
      OneElementPlacementStrategy.Begin[PlacementEffect, BoundUnit, MeasurementUnit],
      OneElementPlacementStrategy.Begin[PlacementEffect, BoundUnit, MeasurementUnit],
    ) 
  )
end layersWidget
