package me.katze.gui4s.layout
package rowcolumn

import bound.Bounds

import cats.Monad
import cats.syntax.all.*

import scala.language.experimental.namedTypeArguments

def updateBoundsWithSizedItem[Place[_], MeasurementUnit : Numeric, T](
                                                                        updateBounds : (Bounds[MeasurementUnit] => Bounds[MeasurementUnit]) => Place[Unit]
                                                                      )(
                                                                        sized: Sized[MeasurementUnit, T],
                                                                        mainAxis : Axis
                                                                      ) : Place[Unit] =
  updateBounds(_.cutAlong(mainAxis, sized.lengthAlong(mainAxis)))
end updateBoundsWithSizedItem

def sizeItems[MeasurementUnit : Numeric, T](items : List[Placed[MeasurementUnit, T]]) : Sized[MeasurementUnit, List[Placed[MeasurementUnit, T]]] =
  val width = items.map(_.endX).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
  val height = items.map(_.endY).maxOption.getOrElse(Numeric[MeasurementUnit].zero)
  new Sized(items, width, height)
end sizeItems


def placeItems[Place[+_] : Monad, MeasurementUnit : Numeric, Item, PlacedItems](
                                                                                  items : List[Place[Sized[MeasurementUnit, Item]]],
                                                                                  mainAxis : Axis,
                                                                                  getBounds : Place[Bounds[MeasurementUnit]],
                                                                                  setBounds : Bounds[MeasurementUnit] => Place[Unit],
                                                                                  placeItemss : (List[Sized[MeasurementUnit, Item]], Bounds[MeasurementUnit]) => PlacedItems
                                                                                ) : Place[PlacedItems] =
  for
    initial <- getBounds
    res <- placeItems(
      items,
      updateBoundsWithSizedItem[MeasurementUnit = MeasurementUnit](f => getBounds.map(f) >>= setBounds)(_, mainAxis),
      placeItemss(_, initial)
    )
    _ <- setBounds(initial)
  yield res
end placeItems

def placeItems[Place[+_] : Monad, SizedItem, PlacedItems](
                                                            items : List[Place[SizedItem]],
                                                            updateBounds : SizedItem => Place[Unit],
                                                            placeItems : List[SizedItem] => PlacedItems
                                                          ) : Place[PlacedItems] =
  measureItems[Place, SizedItem](updateBounds, items).map(placeItems)
end placeItems

def measureItems[Measure[_] : Monad, Item](updateBounds : Item => Measure[Unit], items : List[Measure[Item]]) : Measure[List[Item]] =
  items.foldM(Nil):
    (processedElements, current) =>
        for
          currentItem <- current
          _ <- updateBounds(currentItem)
        yield processedElements :+ currentItem
end measureItems
