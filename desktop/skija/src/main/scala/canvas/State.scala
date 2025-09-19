package gui4s.desktop.skija
package canvas

import canvas.Canvased.applyCanvasFFI

import cats.*
import cats.data.*
import cats.effect.Sync
import cats.syntax.all.*

def saveState[F[_] : {Sync, Canvased}](): F[Int] =
  applyCanvasFFI(_.save())
end saveState

def restoreState[F[_] : {Sync, Canvased}](state: Int): F[Unit] =
  applyCanvasFFI(_.restoreToCount(state))
end restoreState

def stateScoped[F[_] : {Sync, Canvased}, T](f: F[T]): F[T] =
  for
    state <- saveState()
    result <- f
    _ <- restoreState(state)
  yield result
end stateScoped