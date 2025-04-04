package me.katze.gui4s.example
package draw.swing

import api.impl.DrawMonad

import cats.data.ReaderT

import scala.math.Numeric.Implicits.*

type SwingDraw[F[_], MeasurementUnit, T] = ReaderT[F, (MeasurementUnit, MeasurementUnit), T]
type SwingDrawT[F[_], MeasurementUnit] = [T] =>> SwingDraw[F, MeasurementUnit, T]

def runSwingDraw[F[_], MeasurementUnit : Numeric](draw: SwingDraw[F, MeasurementUnit, Unit]): F[Unit] =
  draw.run(Numeric[MeasurementUnit].zero, Numeric[MeasurementUnit].zero)
end runSwingDraw

given [F[_], MeasurementUnit: Numeric] : DrawMonad[SwingDrawT[F, MeasurementUnit], MeasurementUnit] with
  override def move[T](dx: MeasurementUnit, dy: MeasurementUnit, effect: SwingDraw[F, MeasurementUnit, T]): SwingDraw[F, MeasurementUnit, T] =
    ReaderT.apply((x, y) => effect.run(x + dx, y + dy))
  end move
end given