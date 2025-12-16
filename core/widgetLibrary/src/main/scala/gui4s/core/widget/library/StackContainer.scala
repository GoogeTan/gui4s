package gui4s.core.widget.library

import catnip.syntax.additional.*
import catnip.syntax.functor.nestedFunctorsAreFunctors
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.*
import gui4s.core.layout.{Sized, SizedC}
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
  OuterPlace[_] : Monad as OPA,
  Bounds,
  MeasurementUnit : Numeric as MUN
](
  getBounds : OuterPlace[Bounds],
  container : ContainerWidget[
    Widget,
    List,
    OuterPlace * SizedC[MeasurementUnit],
    Point3d[MeasurementUnit]
  ],
)(
   children : List[OuterPlace[Sized[MeasurementUnit, Widget]]],
   xyPlacementStrategy : PlacementStrategy[OuterPlace, Bounds, List, Point2d[MeasurementUnit]],
) : OuterPlace[Sized[MeasurementUnit, Widget]] =
  given Functor[OuterPlace * SizedC[MeasurementUnit]] = nestedFunctorsAreFunctors[OuterPlace, SizedC[MeasurementUnit]]
  given Order[(Point2d[MeasurementUnit], Int)] = Order.by(_._2)
  container(
    children,
    freeChildren =>
      for
        sizedChilden <- freeChildren.traverse[OuterPlace, Sized[MeasurementUnit, Widget]](identity)
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
