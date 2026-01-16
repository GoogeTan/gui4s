package gui4s.android.skia.path

import org.jetbrains.skia.Path
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.*

def containsPoint2d[IO[_] : Sync as S](path : Path, point : Point2d[Float]) : IO[Boolean] =
  S.delay(path.contains(point.x, point.y))
end containsPoint2d

