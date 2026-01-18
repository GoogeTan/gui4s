package gui4s.desktop.skija
package path

import cats.effect.Sync
import io.github.humbleui.skija.Path

import gui4s.core.geometry.Point2d

def containsPoint2d[IO[_] : Sync as S](path : Path, point : Point2d[Float]) : IO[Boolean] =
  S.delay(path.contains(point.x, point.y))
end containsPoint2d

