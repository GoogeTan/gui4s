package me.katze.gui4s.example
package impl

import api.impl.DrawMonad

import cats.data.ReaderT

import scala.math.Numeric.Implicits.*

type Draw[F[_], MeasurementUnit, T] = ReaderT[F, (MeasurementUnit, MeasurementUnit), T]
type DrawT[F[_], MeasurementUnit] = [T] =>> Draw[F, MeasurementUnit, T]

def runDraw[F[_], MeasurementUnit : Numeric](draw: Draw[F, MeasurementUnit, Unit]): F[Unit] =
  draw.run(Numeric[MeasurementUnit].zero, Numeric[MeasurementUnit].zero)
end runDraw

given [F[_], MeasurementUnit: Numeric] : DrawMonad[DrawT[F, MeasurementUnit], MeasurementUnit] with
  override def move[T](dx: MeasurementUnit, dy: MeasurementUnit, effect: Draw[F, MeasurementUnit, T]): Draw[F, MeasurementUnit, T] =
    ReaderT.apply((x, y) => effect.run(x + dx, y + dy))
  end move
end given