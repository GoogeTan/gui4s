package me.katze.gui4s.example
package place

import catnip.syntax.additional.*
import cats.{FlatMap, ~>}
import cats.data.StateT
import me.katze.gui4s.layout.bound.Bounds
import cats.syntax.all.*

type RunPlacement[PlacementEffect[_], F[_]] = PlacementEffect ~> F

def runPlaceStateT[IO[_] : FlatMap, MeasurementUnit](
                                                      bounds : IO[Bounds[MeasurementUnit]]
                                                    ) : RunPlacement[StateT[IO, Bounds[MeasurementUnit], *], IO] =
  new ~>[StateT[IO, Bounds[MeasurementUnit], *], IO]:
    override def apply[A](fa: StateT[IO, Bounds[MeasurementUnit], *][A]): IO[A] =
      bounds
        .flatMap(fa.run)
        .map(_._2)
    end apply
  end new
end runPlaceStateT
