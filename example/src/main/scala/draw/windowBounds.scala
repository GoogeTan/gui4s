package me.katze.gui4s.example
package draw

import cats.*
import cats.syntax.all.*
import me.katze.gui4s.layout.*
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}

def windowBounds[F[_] : Functor, MeasurementUnit : Numeric](window: Window[F, MeasurementUnit]) : F[Bounds[MeasurementUnit]] =
  window.size
    .map((width, height) => Bounds(
      AxisBounds(None, Some(width)),
      AxisBounds(None, Some(height))
    ))
end windowBounds
