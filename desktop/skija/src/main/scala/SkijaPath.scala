package gui4s.desktop.skija

import catnip.ForeignFunctionInterface
import cats.Monad
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, Paint, Path}
import gui4s.core.geometry.Point2d

def containsPoint2d[IO[_]](ffi : ForeignFunctionInterface[IO], path : Path, point : Point2d[Float]) : IO[Boolean] =
  ffi:
    path.contains(point.x, point.y)
end containsPoint2d

def clipPath_[IO[_] : Monad](ffi : ForeignFunctionInterface[IO], canvas: Canvas, path : Path, original : IO[Unit]) : IO[Unit] =
  (
    ffi {
      val state = canvas.save()
      canvas.clipPath(path)
      state
    } <* original
    ).flatMap(state => ffi(canvas.restoreToCount(state)))
end clipPath_

def drawPath[IO[_]](ffi : ForeignFunctionInterface[IO], canvas : Canvas, path : Path, paint: Paint) : IO[Unit] =
  ffi(canvas.drawPath(path, paint))
end drawPath

def clipToPath[IO[_] : Monad](ffi : ForeignFunctionInterface[IO], path: Path, original : SkijaDraw[IO]) : SkijaDraw[IO] =
  SkijaDrawLoud.getCanvas.flatMap:
    clipPath_(ffi.mapK(SkijaDrawLoud.liftK), _, path, original)
end clipToPath
