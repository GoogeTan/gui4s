package gui4s.desktop.widget.library

import catnip.syntax.additional.*
import catnip.syntax.applicative.nestedFunctorsAreFunctors
import cats.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn.*
import gui4s.core.layout.{Sized, SizedT}

@FunctionalInterface
trait StackContainer[Widget]:
  def apply(children : List[Widget]) : Widget

  final def apply(children : Widget*) : Widget =
    this.apply(children.toList)
  end apply
end StackContainer

def stackContainer[
  Update[_] : Monad,
  OuterPlace[_] : Monad as OPA,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  Bounds,
  MeasurementUnit : Numeric as MUN
](
  isEventConsumed : Update[Boolean],
  getBounds : OuterPlace[Bounds],
)(
  children : List[OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * SizedT[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]]],
  xyPlacementStrategy : PlacementStrategy[OuterPlace, Bounds, List, Point2d[MeasurementUnit]],
) : OuterPlace[Sized[MeasurementUnit, Widget[Update, OuterPlace * SizedT[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]] =
  given Functor[OuterPlace * SizedT[MeasurementUnit]] = nestedFunctorsAreFunctors[OuterPlace, SizedT[MeasurementUnit]]
  given Order[(Point2d[MeasurementUnit], Int)] = Order.by(_._2)
  container[
    Update,
    OuterPlace * SizedT[MeasurementUnit],
    List,
    Draw,
    RecompositionReaction,
    HandleableEvent,
    (Point2d[MeasurementUnit], Int)
  ](
    (draw, _) => draw,
    [T] => (update, _) => update,
    isEventConsumed,
    [A, B] => (children : List[A]) =>
      catnip.syntax.list.traverseListOrdered[Update, List, A, B](children)
  )(
    children,
    freeChildren =>
      for
        sizedChilden <- freeChildren.traverse[OuterPlace, Sized[MeasurementUnit, Widget[Update, OuterPlace * SizedT[MeasurementUnit], Draw, RecompositionReaction, HandleableEvent]]](identity)
        childrenSizes = sizedChilden.map(_.size.toPoint2d)
        children = sizedChilden.map(_.value)
        bounds <- getBounds
        placedChildren <- xyPlacementStrategy(childrenSizes, bounds)
      yield Sized(children.zip(placedChildren.coordinatesOfStarts.zipWithIndex), placedChildren.coordinateOfEnd.toRect)
  )
end stackContainer
