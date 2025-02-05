package me.katze.gui4s.example

import place.RunPlacement

import cats.Applicative
import me.katze.gui4s.layout.{Measurable, MeasurableT}
import me.katze.gui4s.layout.bound.Bounds
import cats.syntax.all.*

final class MeasurableRunPlacement[F[_] : Applicative, MeasurementUnit : Numeric](bounds: F[Bounds[MeasurementUnit]]) extends RunPlacement[F, MeasurableT[MeasurementUnit]]:
  override def run[T](toPlace: Measurable[MeasurementUnit, T]): F[T] =
    bounds
      .map(bounds => toPlace.placeInside(bounds))
      .map(_.value)
  end run
end MeasurableRunPlacement
