package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import cats.data.ReaderT
import cats.{Applicative, Monad}
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, Path}
import me.katze.gui4s.geometry.Point2d

def containsPoint2d[IO[_]](ffi : ForeighFunctionInterface[IO], path : Path, point : Point2d[Float]) : IO[Boolean] =
  ffi:
    path.contains(point.x, point.y)
end containsPoint2d

def clipPath_[IO[_] : Monad](ffi : ForeighFunctionInterface[IO], canvas: Canvas, path : Path, original : IO[Unit]) : IO[Unit] =
  (
    ffi {
      val state = canvas.save()
      canvas.clipPath(path)
      state
    } <* original
    ).flatMap(state => ffi(canvas.restoreToCount(state)))
end clipPath_

def clipToPath[IO[_] : Monad, Window](ffi : ForeighFunctionInterface[IO], path: Path, original : SkijaDraw[IO, Window]) : SkijaDraw[IO, Window] =
  ReaderT.ask[IO, SkijaDrawState[IO, Window]].flatMap:
    state =>
      ReaderT.liftF(
        clipPath_[IO](ffi, state.canvas, path, original.run(state))
      )
