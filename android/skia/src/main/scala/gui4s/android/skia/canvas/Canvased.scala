package gui4s.android.skia.canvas

import org.jetbrains.skia.Canvas
import cats.*
import cats.data.*
import cats.effect.*

//TODO refactor me
trait Canvased[F[_]]:
  def canvas: F[Canvas]
end Canvased

object Canvased:
  given[F[_] : Applicative]: Canvased[ReaderT[F, Canvas, *]] with
    override def canvas: ReaderT[F, Canvas, Canvas] =
      ReaderT.ask[F, Canvas]
    end canvas
  end given

  def applyCanvas[F[_] : {FlatMap as FM, Canvased as C}, T](f: Canvas => F[T]): F[T] =
    FM.flatMap(C.canvas)(f)
  end applyCanvas

  def applyCanvasFFI[F[_] : {Sync as S, Canvased}, T](f: Canvas => T): F[T] =
    applyCanvas(canvas => S.delay(f(canvas)))
  end applyCanvasFFI
end Canvased