package gui4s.core.layout

import catnip.Zip
import catnip.Zip.zip
import cats.FlatMap
import cats.Functor
import cats.syntax.all.*

object ContainerStrategy:
  //TODO Имена параметров(и типовых, и обычных) несколько путающие. Не факт, что этот метод вообще должен существовать.
  // Может можно сделать комбинаторы для стратегий получше или что-то типа того.
  def combine[
    Place[_] : FlatMap,
    Collection[_] : {Functor, Zip},
    Size,
    Bounds,
    Point,
    FreeItem,
    MeasuredItem,
    PlacedItem,
  ](
    measurementStrategy: MeasurementStrategy[Place, Collection, FreeItem, MeasuredItem],
    placementStrategy: PlacementStrategy[Place, Size, Size, Bounds, Collection, Point],
    placeItem : Collection[(MeasuredItem, Point)] => Collection[PlacedItem],
    sizeOfItem : MeasuredItem => Size,
  ) : PlacementStrategy[Place, FreeItem, Size, Bounds, Collection, PlacedItem] =
    (items, bounds) =>
      for
        measuredChildren <- measurementStrategy(items)
        placedChildren <- placementStrategy(measuredChildren.map(sizeOfItem), bounds)
      yield placedChildren.copy(value = placeItem(measuredChildren.zip(placedChildren.value)))
  end combine
end ContainerStrategy

