package gui4s.desktop.skija
package canvas

import canvas.Canvased.applyCanvasFFI

import catnip.ForeignFunctionInterface
import cats.*
import cats.data.*
import cats.syntax.all.*

def saveState[F[_] : {FlatMap, ForeignFunctionInterface, Canvased}](): F[Int] =
  applyCanvasFFI(_.save())
end saveState

def restoreState[F[_] : {FlatMap, ForeignFunctionInterface, Canvased}](state: Int): F[Unit] =
  applyCanvasFFI(_.restoreToCount(state))
end restoreState

def stateScoped[F[_] : {FlatMap, ForeignFunctionInterface, Canvased}, T](f: F[T]): F[T] =
  for
    state <- saveState()
    result <- f
    _ <- restoreState(state)
  yield result
end stateScoped