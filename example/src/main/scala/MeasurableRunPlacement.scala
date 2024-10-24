package me.katze.gui4s.example

import place.RunPlacement

import cats.Applicative
import me.katze.gui4s.layout.{Measurable, MeasurableT}
import me.katze.gui4s.layout.bound.Bounds
import cats.syntax.all.{*, given}

final class MeasurableRunPlacement[F[_] : Applicative, MU : Numeric](bounds: F[Bounds[MU]]) extends RunPlacement[F, MeasurableT[MU]]:
  override def run[T](toPlace: Measurable[MU, T]): F[T] =
    bounds
      .map(bounds => toPlace.placeInside(bounds))
      .map(_.value)
  end run
end MeasurableRunPlacement
