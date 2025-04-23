package me.katze.gui4s.example
package draw.swing

import api.impl.DrawMonad

import cats.data.ReaderT

import scala.math.Numeric.Implicits.*

final case class SwingDrawState[MU](x : MU, y : MU)

type SwingDraw[F[_], MeasurementUnit, T] = ReaderT[F, SwingDrawState[MeasurementUnit], T]
type SwingDrawT[F[_], MeasurementUnit] = [T] =>> SwingDraw[F, MeasurementUnit, T]

def runSwingDraw[F[+_], MeasurementUnit : Numeric](draw: SwingDraw[F, MeasurementUnit, Unit]): F[Unit] =
  draw.run(SwingDrawState(Numeric[MeasurementUnit].zero, Numeric[MeasurementUnit].zero))
end runSwingDraw

given [F[+_], MeasurementUnit: Numeric] : DrawMonad[SwingDraw[F, MeasurementUnit, Unit], MeasurementUnit] with
  override def move(dx: MeasurementUnit, dy: MeasurementUnit, effect: SwingDraw[F, MeasurementUnit, Unit]): SwingDraw[F, MeasurementUnit, Unit] =
    ReaderT.apply(s => effect.run(s.copy(x = s.x + dx, y = s.y + dy)))
  end move
end given