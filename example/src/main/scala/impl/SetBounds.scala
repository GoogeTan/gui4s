package me.katze.gui4s.example
package impl

import catnip.syntax.additional.*
import cats.Applicative
import cats.data.StateT
import me.katze.gui4s.layout.bound.Bounds

type SetBounds[F[_], MeasurementUnit] = Bounds[MeasurementUnit] => F[Unit]

def setBoundsStateT[F[_] : Applicative, MeasurementUnit] : SetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
  StateT.set
end setBoundsStateT
