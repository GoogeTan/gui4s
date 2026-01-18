package gui4s.core.widget.library

import catnip.syntax.additional._
import cats._
import cats.syntax.all._

import gui4s.core.geometry._
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
import gui4s.core.layout.rowcolumn.PlacementStrategy
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

def layersWidget[
  Widget,
  OuterPlace[_] : Monad as OPA,
  MeasurementUnit : Numeric as MUN
](
  container : ContainerWidget[
    Widget,
    List,
    OuterPlace * SizedC[MeasurementUnit],
    Point3d[MeasurementUnit]
  ]
)(
   background : List[OuterPlace[Sized[MeasurementUnit, Widget]]],
   foreground : List[OuterPlace[Sized[MeasurementUnit, Widget]]],
   decorationsPlacementStrategy : PlacementStrategy[OuterPlace, Rect[MeasurementUnit], List, Point2d[MeasurementUnit]],
) : Decorator[
  OuterPlace[Sized[MeasurementUnit, Widget]]
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
