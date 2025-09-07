package gui4s.desktop.widget.library

import catnip.syntax.additional.*
import cats.*
import gui4s.core.geometry.Point3d
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.OneElementPlacementStrategy

@FunctionalInterface
trait StackContainer[Widget]:
  def apply(children : List[Widget]) : Widget

  final def apply(children : Widget*) : Widget =
    this.apply(children.toList)
  end apply
end StackContainer

def stackContainer[
  Update[_] : Monad,
  Place[_] : Functor,
  Draw : Monoid,
  RecompositionReaction : Monoid,
  HandleableEvent,
  BoundsUnit,
  MeasurementUnit
](
  isEventConsumed : Update[Boolean]
)(
  children : List[Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]]],
  horizontalPlacementStrategy : OneElementPlacementStrategy[Place, BoundsUnit, MeasurementUnit],
  verticalPlacementStrategy   : OneElementPlacementStrategy[Place, BoundsUnit, MeasurementUnit],
) : Place[Widget[Update, Place, Draw, RecompositionReaction, HandleableEvent]] =
  container[
    Update,
    Place,
    List,
    Draw,
    RecompositionReaction,
    HandleableEvent,
    Int
  ](
    (draw, _) => draw,
    [T] => (update, _) => update,
    isEventConsumed,
    [A, B] => (children : List[A]) =>
      catnip.syntax.list.orderedListProcessing[Update, List, A, B](children)
  )(
    children,
    freeChildren =>
      ???
  )
end stackContainer
