package me.katze.gui4s.example
package place

import catnip.syntax.additional.*
import cats.FlatMap
import cats.data.StateT
import me.katze.gui4s.layout.bound.Bounds
import cats.syntax.all.*

type RunPlacement[PlacementEffect[_], F[_]] = [Value] => PlacementEffect[Value] => F[Value]

def runPlaceStateT[IO[_] : FlatMap, MeasurementUnit](
                                                      bounds : IO[Bounds[MeasurementUnit]]
                                                    ) : RunPlacement[StateT[IO, Bounds[MeasurementUnit], *], IO] =
  [Value] => (toPlace : StateT[IO, Bounds[MeasurementUnit], Value]) =>
    bounds
      .flatMap(bounds => toPlace.run(bounds))
      .map(_._2)
end runPlaceStateT
