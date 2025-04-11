package me.katze.gui4s.example

import place.RunPlacement

import cats.{InjectK, Monad}
import me.katze.gui4s.layout.{Measurable, MeasurableT}
import me.katze.gui4s.layout.bound.Bounds
import cats.syntax.all.*

final class MeasurableRunPlacement[F[_] : Monad, G[+_], MeasurementUnit : Numeric](
                                                                                    bounds: F[Bounds[MeasurementUnit]]
                                                                                  )(
                                                                                      using I : InjectK[G, F]
                                                                                    ) extends RunPlacement[F, MeasurableT[G, MeasurementUnit]]:
  override def run[T](toPlace: Measurable[G, MeasurementUnit, T]): F[T] =
    bounds
      .flatMap(bounds => I(toPlace(bounds)))
      .map(_.value)
  end run
end MeasurableRunPlacement
