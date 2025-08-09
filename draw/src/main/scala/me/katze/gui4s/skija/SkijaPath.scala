package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import cats.data.ReaderT
import cats.{Applicative, Monad}
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, Paint, Path}
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

def drawPath[IO[_]](ffi : ForeighFunctionInterface[IO], canvas : Canvas, path : Path, paint: Paint) : IO[Unit] =
  ffi(canvas.drawPath(path, paint))
end drawPath

def clipToPath[IO[_] : Monad](ffi : ForeighFunctionInterface[IO], path: Path, original : SkijaDraw[IO]) : SkijaDraw[IO] =
  SkijaDrawLoud.getCanvas.flatMap:
    clipPath_(ffi.mapK(SkijaDrawLoud.liftK), _, path, original)
end clipToPath
