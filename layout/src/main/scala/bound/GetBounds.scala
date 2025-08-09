package me.katze.gui4s.layout
package bound

import bound.Bounds

import catnip.Get
import cats.Applicative
import cats.arrow.FunctionK
import cats.data.{ReaderT, StateT}

type GetBounds[F[_], MeasurementUnit] = Get[F, Bounds[MeasurementUnit]]

object GetBounds:
  def liftK[F[_], G[_], MeasurementUnit](original : GetBounds[F, MeasurementUnit], f : FunctionK[F, G]) : GetBounds[G, MeasurementUnit] =
    f(original)
  end liftK

  def stateT[F[_] : Applicative, MeasurementUnit] : GetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
    Get.stateT[F, Bounds[MeasurementUnit]]
  end stateT

  def readerT[F[_] : Applicative, MeasurementUnit] : GetBounds[ReaderT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
    Get.readerT[F, Bounds[MeasurementUnit]]
  end readerT
end GetBounds
