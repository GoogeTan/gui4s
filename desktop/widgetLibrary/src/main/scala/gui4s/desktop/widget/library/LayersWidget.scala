package gui4s.desktop.widget.library

import catnip.syntax.additional.*
import cats.syntax.all.*
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
  original =>
    container(
      background ++ (original :: foreground),
      elements =>
        for
          sizedElements <- elements.traverse(identity)
          originalSized = sizedElements(background.size)
          backgroundSizes = sizedElements.take(background.size).map(_.size.toPoint2d)
          foregroundSizes = sizedElements.takeRight(foreground.size).map(_.size.toPoint2d)
          placedElements <- decorationsPlacementStrategy(backgroundSizes ++ foregroundSizes, originalSized.size)
          elementsCoodinates = placedElements.coordinatesOfStarts
          shifts = (
            elementsCoodinates.take(background.size)
              ++ (Point2d(MUN.zero, MUN.zero) :: elementsCoodinates.takeRight(foreground.size))
          ).mapWithIndex((point, index) => new Point3d(point, MUN.fromInt(index)))
        yield Sized(sizedElements.map(_.value).zip(shifts), originalSized.size)
    )
end layersWidget
