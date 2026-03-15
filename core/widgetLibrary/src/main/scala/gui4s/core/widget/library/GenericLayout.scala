package gui4s.core.widget.library

import catnip.syntax.additional._
import cats.Functor
import cats.Monad
import cats.syntax.all._

import gui4s.core.layout.PlacementStrategy
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC

class GenericLayout[
  PlacementEffect[_] : Monad as OPA,
  Collection[_] : Functor,
  PlacedWidget,
  FreeWidget,
  Size,
  Bounds,
  IncrementalFreeWidget,
  NewFreeWidget
](
  measureWithBounds : NewFreeWidget => FreeWidget,
  measureWithBoundsIncrementally : IncrementalFreeWidget => FreeWidget,
  getBounds : PlacementEffect[Bounds],
  placementStrategy: PlacementStrategy[
    PlacementEffect,
    FreeWidget,
    Size,
    Bounds,
    Collection,
    PlacedWidget
  ],
) extends Layout[
  PlacementEffect * SizedC[Size],
  Collection,
  NewFreeWidget,
  IncrementalFreeWidget,
  PlacedWidget
]:
  override def place(
    widgets: Collection[NewFreeWidget]
  ): PlacementEffect[
    Sized[
      Size,
      Collection[PlacedWidget]
    ]
  ] =
    for
      bounds <- getBounds
      measuredElements <- placementStrategy(widgets.map(measureWithBounds), bounds)
    yield Sized(
      measuredElements.coordinates, 
      measuredElements.size
    )
  end place

  override def placeIncrementally(
    widgets: Collection[IncrementalFreeWidget]
  ): PlacementEffect[Sized[Size, Collection[PlacedWidget]]] =
    for
      bounds <- getBounds
      measuredElements <- placementStrategy(widgets.map(measureWithBoundsIncrementally), bounds)
    yield Sized(measuredElements.coordinates, measuredElements.size)
  end placeIncrementally
end GenericLayout