package me.katze.gui4s.widget.library

import catnip.Zip
import catnip.syntax.additional.*
import cats.syntax.all.*
import cats.{Applicative, Monad, Traverse}
import me.katze.gui4s.geometry.{Axis, InfinityOr, Point3d}
import me.katze.gui4s.layout.bound.{GetBounds, SetBounds}
import me.katze.gui4s.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy, rowColumnLayoutPlacement}
import me.katze.gui4s.layout.{Placed, Sized}

@FunctionalInterface
trait LinearContainer[
  Widget,
  Place[_],
  Container[_],
  MeasurementUnit,
  Axis,
]:
  def apply(
              children               : Container[Widget],
              mainAxis               : Axis,
              mainAxisStrategy       : ManyElementsPlacementStrategy[Place, InfinityOr[MeasurementUnit], Container, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, InfinityOr[MeasurementUnit], MeasurementUnit],
            ) : Widget
end LinearContainer

def linearContainer[
  PlacedWidget,
  Place[_] : Monad,
  Container[_] : {Applicative as A, Traverse, Zip},
  MeasurementUnit : Numeric,
](
  container : ContainerWidget[PlacedWidget, Container, Place * Sized[MeasurementUnit, *], Point3d[MeasurementUnit]],
  getBounds: GetBounds[Place, MeasurementUnit],
  setBounds: SetBounds[Place, MeasurementUnit]
) : LinearContainer[Place[Sized[MeasurementUnit, PlacedWidget]], Place, Container, MeasurementUnit, Axis] =
  (children, mainAxis, mainAxisStrategy, additionalAxisStrategy) =>
    container(
      children,
      freeChildren =>
        rowColumnLayoutPlacement[
          Place, Container, PlacedWidget, MeasurementUnit
        ](
          getBounds,
          setBounds,
          mainAxis,
          freeChildren,
          ManyElementsPlacementStrategy.Zip(
            mainAxis,
            mainAxisStrategy,
            ManyElementsPlacementStrategy.OneByOne(
              additionalAxisStrategy
            )
          )
        ).map(_.mapValue(elements => A.map(elements)(placedElementAsLayoutMetadata)))
    )
end linearContainer

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, Point3d[MeasurementUnit]) =
  (placed.value, placed.coordinate)
end placedElementAsLayoutMetadata
