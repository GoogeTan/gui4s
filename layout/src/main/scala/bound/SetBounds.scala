package me.katze.gui4s.layout
package bound

import bound.Bounds

import catnip.Set
import cats.Applicative
import cats.arrow.FunctionK
import cats.data.{StateT, WriterT}

type SetBounds[F[_], MeasurementUnit] = Set[F, Bounds[MeasurementUnit]]

object SetBounds:
  def liftK[F[_], G[_], MeasurementUnit](original : SetBounds[F, MeasurementUnit], f : FunctionK[F, G]) : SetBounds[G, MeasurementUnit] =
    bounds => f(original(bounds))
  end liftK
  
  def stateT[F[_] : Applicative, MeasurementUnit] : SetBounds[StateT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
    Set.stateT[F, Bounds[MeasurementUnit]]
  end stateT
  
  def writerT[F[_] : Applicative, MeasurementUnit] : SetBounds[WriterT[F, Bounds[MeasurementUnit], *], MeasurementUnit] =
    Set.writerT[F, Bounds[MeasurementUnit]]
  end writerT
end SetBounds