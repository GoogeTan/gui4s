package gui4s.desktop.widget.library

import catnip.syntax.additional._
import cats._

import gui4s.core.geometry._
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

def layersWidget[
  Update[_] : Monad,
  PlacementEffect[_] : Monad as OPA,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  MeasurementUnit : Numeric as MUN
](
  container : ContainerWidget[
    Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent],
    List,
    PlacementEffect * SizedC[MeasurementUnit],
    Point3d[MeasurementUnit]
  ]
)(
   background : List[PlacementEffect[Sized[MeasurementUnit, Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]],
   foreground : List[PlacementEffect[Sized[MeasurementUnit, Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]],
   decorationsPlacementStrategy : PlacementStrategy[PlacementEffect, Rect[MeasurementUnit], Rect[MeasurementUnit], List, Point2d[MeasurementUnit]],
) : Decorator[
  PlacementEffect[Sized[MeasurementUnit, Widget[Update, PlacementEffect * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]
] =
  gui4s.core.widget.library.layersWidget(container)(background, foreground, decorationsPlacementStrategy)
end layersWidget
