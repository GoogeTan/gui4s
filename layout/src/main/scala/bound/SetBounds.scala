package me.katze.gui4s.layout
package bound

import bound.Bounds

import cats.*
import cats.data.StateT

type SetBounds[F[_], MeasurementUnit] = Bounds[MeasurementUnit] => F[Unit]

object SetBounds:
    def setBoundsStateT[F[_] : Applicative, MeasurementUnit] : SetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
        StateT.set
    end setBoundsStateT

    def liftK[F[_], G[_], MeasurementUnit](original : SetBounds[F, MeasurementUnit], f : F ~> G) : SetBounds[G, MeasurementUnit] =
        bounds =>
            f(original(bounds))
    end liftK
end SetBounds
