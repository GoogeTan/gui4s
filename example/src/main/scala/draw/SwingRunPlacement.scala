package me.katze.gui4s.example
package draw

import place.RunPlacement

import cats.*
import me.katze.gui4s.layout.Measurable
import me.katze.gui4s.layout.bound.{AxisBounds, Bounds}
import cats.syntax.all.{*, given}
import me.katze.gui4s.layout.{*, given}

class SwingRunPlacement[F[_] : Applicative, MU : Numeric](api: SwingApi[F]) extends RunPlacement[F, MeasurableT[MU]]:
  override def run[T](toPlace: Measurable[MU, T]): F[T] =
    api.size
      .map((width, height) => Bounds(
        AxisBounds(None, Some(Numeric[MU].fromInt(width))),
        AxisBounds(None, Some(Numeric[MU].fromInt(height)))
      ))
      .map(toPlace.placeInside)
      .map(_.value)
  end run
end SwingRunPlacement