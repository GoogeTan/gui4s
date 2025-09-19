package gui4s.desktop.skija
package canvas

import catnip.{ForeignFunctionInterface, Get}
import cats.FlatMap
import io.github.humbleui.skija.Canvas

type Canvased[F[_]] = Get[F, Canvas]

object Canvased:
  def applyCanvas[F[_] : {FlatMap as FM, Canvased as C}, T](f: Canvas => F[T]): F[T] =
    FM.flatMap(C)(f)
  end applyCanvas

  def applyCanvasFFI[F[_] : {FlatMap, ForeignFunctionInterface as ffi, Canvased}, T](f: Canvas => T): F[T] =
    applyCanvas(canvas => ffi(f(canvas)))
  end applyCanvasFFI
end Canvased