package gui4s.desktop.skija
package canvas

import cats.FlatMap
import cats.syntax.all.*
import cats.effect.*
import cats.effect.syntax.*
import cats.effect.std.*

def transition[F[_] : {Sync, Canvased}](x: Float, y: Float): F[Unit] =
  Canvased.applyCanvasFFI(_.translate(x, y))
end transition


def drawAt[F[_] : {Sync, Canvased}, T](x: Float, y: Float, value: F[T]): F[T] =
  stateScoped(transition(x, y) *> value)
end drawAt
