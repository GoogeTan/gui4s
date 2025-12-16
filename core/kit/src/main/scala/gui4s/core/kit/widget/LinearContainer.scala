package gui4s.core.kit.widget

import catnip.syntax.additional.*
import catnip.{Get, Set, Zip}
import cats.syntax.all.*
import cats.{Applicative, Monad, Traverse}
import gui4s.core.geometry.{Axis, Point3d, Rect}
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy, rowColumnLayoutPlacement}
import gui4s.core.layout.{Placed, Sized}

@FunctionalInterface
trait LinearContainer[
  Widget,
  Place[_],
  Container[_],
  BoundUnit,
  MeasurementUnit,
  Axis,
]:
  def apply(
              children               : Container[Widget],
              mainAxis               : Axis,
              mainAxisStrategy       : PlacementStrategy[Place, BoundUnit, Container, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, BoundUnit, MeasurementUnit],
            ) : Widget
end LinearContainer

def linearContainer[
  PlacedWidget,
  Place[_] : Monad,
  Container[_] : {Applicative as A, Traverse, Zip},
  BoundUnit,
  MeasurementUnit : Numeric as N,
](
  container : ContainerWidget[PlacedWidget, Container, Place * Sized[MeasurementUnit, *], Point3d[MeasurementUnit]],
  getBounds: Get[Place, Rect[BoundUnit]],
  setBounds: Set[Place, Rect[BoundUnit]],
  cut : (BoundUnit, MeasurementUnit) => BoundUnit 
) : LinearContainer[Place[Sized[MeasurementUnit, PlacedWidget]], Place, Container, BoundUnit, MeasurementUnit, Axis] =
  (children, mainAxis, mainAxisStrategy, additionalAxisStrategy) =>
    container(
      children,
      freeChildren =>
        rowColumnLayoutPlacement[
          Place, Container, PlacedWidget, BoundUnit, MeasurementUnit
        ](
          getBounds,
          setBounds,
          cut,
          mainAxis,
          freeChildren,
          PlacementStrategy.Zip(
            mainAxis,
            mainAxisStrategy,
            PlacementStrategy.PlaceIndependently(
              additionalAxisStrategy,
              N.zero
            )
          ),
        ).map(_.mapValue(elements => A.map(elements)(placedElementAsLayoutMetadata)))
    )
end linearContainer

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, Point3d[MeasurementUnit]) =
  (placed.value, placed.coordinate)
end placedElementAsLayoutMetadata
