package me.katze.gui4s.example
package impl

import catnip.syntax.additional.*
import cats.Applicative
import cats.data.StateT
import me.katze.gui4s.layout.bound.Bounds

type GetBounds[F[_], MeasurementUnit] = F[Bounds[MeasurementUnit]]

def getBoundsStateT[F[_] : Applicative, MeasurementUnit] : GetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
  StateT.get
end getBoundsStateT
