package me.katze.gui4s.layout
package rowcolumn

import bound.{AxisDependentBounds, Bounds}

import cats.Monad
import cats.syntax.all.*

import scala.math.Fractional.*
import scala.math.Fractional.Implicits.*

def weightedRowColumnPlace[F[+_] : Monad, MeasurementUnit : Fractional, T](
                                                                            mainAxis: Axis,
                                                                            elements : List[MaybeWeighted[Measurable[F, MeasurementUnit, T]]],
                                                                            rowColumnPlace : (List[Sized[MeasurementUnit, T]], AxisDependentBounds[MeasurementUnit]) => List[Placed[MeasurementUnit, T]]
                                                                          ): Measurable[F, MeasurementUnit, List[Placed[MeasurementUnit, T]]] =
  constraints =>
    val dependentAxes = AxisDependentBounds.fromConstraints(constraints, mainAxis)
    measure(elements, dependentAxes).map:
      measured =>
        val placed = rowColumnPlace(measured, dependentAxes)
        val width = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
        val height = placed.map(a => a.x + a.width).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
        new Sized(placed, width, height)
end weightedRowColumnPlace

def updateBoundsWithSizedItem[Place[_], MeasurementUnit : Numeric, T](
                                                                        updateBounds : (Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) => Place[Unit]
                                                                     )(
                                                                        sized: Sized[MeasurementUnit, T],
                                                                        mainAxis : Axis
                                                                      ) : Place[Unit] =
  updateBounds(_.cutAlong(mainAxis, sized.lengthAlong(mainAxis)))
end updateBoundsWithSizedItem

def rowColumnPlaceSized[Place[+_] : Monad, MeasurementUnit : Numeric, Item, PlacedItems](
                                                                                           items : List[Place[Sized[MeasurementUnit, Item]]],
                                                                                           mainAxis : Axis,
                                                                                           updateBounds : (Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) => Place[Unit],
                                                                                           placeItems : List[Sized[MeasurementUnit, Item]] => Place[PlacedItems]
                                                                                         ) : Place[PlacedItems] =
  rowColumnPlace(
    items,
    updateBoundsWithSizedItem(updateBounds)(_, mainAxis),
    placeItems
  )
end rowColumnPlaceSized

def rowColumnPlace[Place[+_] : Monad, SizedItem, PlacedItems](
                                                              items : List[Place[SizedItem]],
                                                              updateBounds : SizedItem => Place[Unit],
                                                              placeItems : List[SizedItem] => Place[PlacedItems]
                                                              ): Place[PlacedItems] =
  measureItems[Place, SizedItem](updateBounds, items) >>= placeItems
end rowColumnPlace

def measureItems[Measure[_] : Monad, Item](updateBounds : Item => Measure[Unit], items : List[Measure[Item]]) : Measure[List[Item]] =
  items.foldM(Nil):
    (processedElements, current) =>
        for
          currentItem <- current
          _ <- updateBounds(currentItem)
        yield processedElements :+ currentItem
end measureItems
