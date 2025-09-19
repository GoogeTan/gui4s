package gui4s.desktop.skija
package path

import catnip.ForeignFunctionInterface
import gui4s.core.geometry.Point2d
import io.github.humbleui.skija.Path

def containsPoint2d[IO[_] : ForeignFunctionInterface as ffi](path : Path, point : Point2d[Float]) : IO[Boolean] =
  ffi(path.contains(point.x, point.y))
end containsPoint2d

