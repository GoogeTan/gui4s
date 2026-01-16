package gui4s.android.skia.canvas

import org.jetbrains.skia.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

def transition[F[_] : {Sync, Canvased}](x: Float, y: Float): F[Unit] =
  Canvased.applyCanvasFFI(_.translate(x, y))
end transition


def drawAt[F[_] : {Sync, Canvased}, T](x: Float, y: Float, value: F[T]): F[T] =
  stateScoped(transition(x, y) *> value)
end drawAt
