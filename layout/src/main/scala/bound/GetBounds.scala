package me.katze.gui4s.layout
package bound

import bound.Bounds

import cats.*
import cats.data.{ReaderT, StateT}

type GetBounds[F[_], MeasurementUnit] = F[Bounds[MeasurementUnit]]

object GetBounds:
    def getBoundsStateT[F[_] : Applicative, MeasurementUnit] : GetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
    StateT.get
    end getBoundsStateT

    def getBoundsReaderT[F[_] : Applicative, MeasurementUnit] : GetBounds[ReaderT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
    ReaderT.ask
    end getBoundsReaderT

    def liftK[F[_], G[_], MeasurementUnit](original : GetBounds[F, MeasurementUnit], f : F ~> G) : GetBounds[G, MeasurementUnit] =
        f(original)
    end liftK
end GetBounds
