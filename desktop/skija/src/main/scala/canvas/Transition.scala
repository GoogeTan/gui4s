package gui4s.desktop.skija
package canvas

import catnip.ForeignFunctionInterface
import cats.FlatMap
import cats.syntax.all.*

def transition[F[_] : {FlatMap, ForeignFunctionInterface, Canvased}](x: Float, y: Float): F[Unit] =
  Canvased.applyCanvasFFI(_.translate(x, y))
end transition


def drawAt[F[_] : {FlatMap, ForeignFunctionInterface, Canvased}, T](x: Float, y: Float, value: F[T]): F[T] =
  stateScoped(transition(x, y) *> value)
end drawAt
