package gui4s.core.widget.library

import catnip.syntax.additional.*
import cats.syntax.all.*
import cats.{Functor, Monad}
import gui4s.core.geometry.Rect
import gui4s.core.layout.{PlacementStrategy, Sized, SizedC}

class GenericLayout[
  PlacementEffect[_] : Monad as OPA,
  Collection[_] : Functor,
  PlacedWidget,
  FreeWidget,
  MeasurementUnit : Numeric as MUN,
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
    Rect[MeasurementUnit],
    Bounds,
    Collection,
    PlacedWidget
  ],
) extends Layout[
  PlacementEffect * SizedC[MeasurementUnit],
  Collection,
  NewFreeWidget,
  IncrementalFreeWidget,
  PlacedWidget
]:
  override def place(
    widgets: Collection[NewFreeWidget]
  ): PlacementEffect[
    Sized[
      MeasurementUnit,
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
  ): PlacementEffect[Sized[MeasurementUnit, Collection[PlacedWidget]]] =
    for
      bounds <- getBounds
      measuredElements <- placementStrategy(widgets.map(measureWithBoundsIncrementally), bounds)
    yield Sized(measuredElements.coordinates, measuredElements.size)
  end placeIncrementally
end GenericLayout