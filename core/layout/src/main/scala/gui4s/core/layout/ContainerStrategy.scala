package gui4s.core.layout

import catnip.Zip
import catnip.Zip.zip
import cats.FlatMap
import cats.Functor
import cats.syntax.all.*

object ContainerStrategy:
  def combine[
    Place[_] : FlatMap,
    Collection[_] : {Functor, Zip},
    Size,
    Bounds,
    Point,
    FreeItem,
    PlacedItem,
    Res,
  ](
    measurementStrategy: MeasurementStrategy[Place, Collection, FreeItem, PlacedItem],
    placementStrategy: PlacementStrategy[Place, Size, Size, Bounds, Collection, Point],
    someMap : Collection[(PlacedItem, Point)] => Collection[Res],
    sizeOfItem : PlacedItem => Size,
  ) : PlacementStrategy[Place, FreeItem, Size, Bounds, Collection, Res] =
    (items, bounds) =>
      for 
        measuredChildren <- measurementStrategy(items)
        placedChildren <- placementStrategy(measuredChildren.map(sizeOfItem), bounds)
      yield placedChildren.copy(value = someMap(measuredChildren.zip(placedChildren.value)))
  end combine
end ContainerStrategy

