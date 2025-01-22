package me.katze.gui4s.example
package impl

import api.impl.DrawMonad

import cats.data.ReaderT

import scala.math.Numeric.Implicits.*

type Draw[F[_], MU, T] = ReaderT[F, (MU, MU), T]
type DrawT[F[_], MU] = [T] =>> Draw[F, MU, T]

def runDraw[F[_], MU : Numeric](draw: Draw[F, MU, Unit]): F[Unit] =
  draw.run(Numeric[MU].zero, Numeric[MU].zero)
end runDraw

given [F[_], MU: Numeric] : DrawMonad[DrawT[F, MU], MU] with
  override def move[T](dx: MU, dy: MU, effect: Draw[F, MU, T]): Draw[F, MU, T] =
    ReaderT.apply((x, y) => effect.run(x + dx, y + dy))
  end move
end given