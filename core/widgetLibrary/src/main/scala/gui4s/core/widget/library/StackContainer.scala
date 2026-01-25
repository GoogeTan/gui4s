package gui4s.core.widget.library

import catnip.syntax.additional._
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats._
import cats.syntax.all._

import gui4s.core.geometry._
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC
import gui4s.core.layout.rowcolumn._
import gui4s.core.widget.library.ContainerWidget

@FunctionalInterface
trait StackContainer[Widget]:
  def apply(children : List[Widget]) : Widget

  final def apply(children : Widget*) : Widget =
    this.apply(children.toList)
  end apply
end StackContainer

def stackContainer[
  Widget,
  PlacementEffect[_] : Monad as OPA,
  Bounds,
  MeasurementUnit : Numeric as MUN
](
  getBounds : PlacementEffect[Bounds],
  container : ContainerWidget[
    Widget,
    List,
    PlacementEffect * SizedC[MeasurementUnit],
    Point3d[MeasurementUnit]
  ],
)(
   children : List[PlacementEffect[Sized[MeasurementUnit, Widget]]],
   xyPlacementStrategy : PlacementStrategy[PlacementEffect, Bounds, List, Point2d[MeasurementUnit]],
) : PlacementEffect[Sized[MeasurementUnit, Widget]] =
  given Functor[PlacementEffect * SizedC[MeasurementUnit]] = nestedFunctorsAreFunctors[PlacementEffect, SizedC[MeasurementUnit]]
  given Order[(Point2d[MeasurementUnit], Int)] = Order.by(_._2)
  container(
    children,
    freeChildren =>
      for
        sizedChilden <- freeChildren.traverse[PlacementEffect, Sized[MeasurementUnit, Widget]](identity)
        childrenSizes = sizedChilden.map(_.size.toPoint2d)
        children = sizedChilden.map(_.value)
        bounds <- getBounds
        placedChildren <- xyPlacementStrategy(childrenSizes, bounds)
      yield Sized(
        children.zip(
          placedChildren
            .coordinatesOfStarts
            .zipWithIndex.map((coordinate2d, index) => new Point3d(coordinate2d, MUN.fromInt(index)))
        ), 
        placedChildren.coordinateOfEnd.toRect
      )
  )
end stackContainer
