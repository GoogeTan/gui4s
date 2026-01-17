package gui4s.desktop.widget.library

import catnip.syntax.additional.*
import cats.*
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.layout.{Sized, SizedC}
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

def layersWidget[
  Update[_] : Monad,
  OuterPlace[_] : Monad as OPA,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  MeasurementUnit : Numeric as MUN
](
  container : ContainerWidget[
    Widget[Update, OuterPlace * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent],
    List,
    OuterPlace * SizedC[MeasurementUnit],
    Point3d[MeasurementUnit]
  ]
)(
   background : List[OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]],
   foreground : List[OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]],
   decorationsPlacementStrategy : PlacementStrategy[OuterPlace, Rect[MeasurementUnit], List, Point2d[MeasurementUnit]],
) : Decorator[
  OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * SizedC[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]
] =
  gui4s.core.widget.library.layersWidget(container)(background, foreground, decorationsPlacementStrategy)
end layersWidget
