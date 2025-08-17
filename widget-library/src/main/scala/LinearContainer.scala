package me.katze.gui4s.widget.library

import catnip.syntax.additional.*
import cats.syntax.all.*
import cats.{Applicative, Monad, Traverse}
import me.katze.gui4s.geometry.{Axis, Point3d}
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
              mainAxisStrategy       : ManyElementsPlacementStrategy[Place, Container, MeasurementUnit],
              additionalAxisStrategy : OneElementPlacementStrategy[Place, MeasurementUnit],
            ) : Widget
end LinearContainer

def linearContainer[
  PlacedWidget,
  Place[_] : Monad,
  Container[_] : {Applicative as A, Traverse},
  MeasurementUnit : Numeric,
](
  container : ContainerWidget[PlacedWidget, Container, Place * Sized[MeasurementUnit, *], Point3d[MeasurementUnit]],
  getBounds: GetBounds[Place, MeasurementUnit],
  setBounds: SetBounds[Place, MeasurementUnit],
  zip : [A, B] => (Container[A], Container[B]) => Container[(A, B)]
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
          mainAxisStrategy,
          additionalAxisStrategy,
          zip
        ).map(_.mapValue(elements => A.map(elements)(placedElementAsLayoutMetadata)))
    )
end linearContainer

def placedElementAsLayoutMetadata[MeasurementUnit, T](placed : Placed[MeasurementUnit, T]) : (T, Point3d[MeasurementUnit]) =
  (placed.value, placed.coordinate)
end placedElementAsLayoutMetadata
