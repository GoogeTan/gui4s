package me.katze.gui4s.example
package impl

import api.impl.{DrawMonad, LayoutPlacementMeta}

import cats.Applicative
import cats.data.ReaderT
import cats.effect.IO
import cats.syntax.all.*
import me.katze.gui4s.widget.library.LayoutDraw

import scala.math.Numeric.Implicits.*

type Draw[MU, T] = ReaderT[IO, (MU, MU), T]
type DrawT[MU] = [T] =>> Draw[MU, T]

def runDraw[MU : Numeric](draw: DrawT[MU][Unit]): IO[Unit] =
  draw.run(Numeric[MU].zero, Numeric[MU].zero)
end runDraw

given [MU: Numeric] : DrawMonad[DrawT[MU], MU] with
  override def move[T](dx: MU, dy: MU, effect: Draw[MU, T]): Draw[MU, T] =
    ReaderT.apply((x, y) => effect.run(x + dx, y + dy))
  end move
end given