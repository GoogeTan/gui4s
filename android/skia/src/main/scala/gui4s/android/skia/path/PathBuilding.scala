package gui4s.android.skia.path

import org.jetbrains.skia.{Path, Rect as SkRect, RRect as SkRRect, PathDirection}
import cats.*
import cats.arrow.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.*

type PathBuilding[F[_]] = Kleisli[F, Path, Path]

def pathBuildingFFI[F[_] : Sync as S](f : Path => Path) : PathBuilding[F] =
  Kleisli(path => S.delay(f(path)))
end pathBuildingFFI

def addOval[F[_] : Sync](x : Float, y : Float, width : Float, height : Float) : PathBuilding[F] =
  pathBuildingFFI(
    _.addOval(
      SkRect.Companion.makeXYWH(x, y, width, height),
      PathDirection.CLOCKWISE,
      1
    )
  )
end addOval

def addRRect[F[_] : Sync](where : Point2d[Float], rrect : RRect[Float]) : PathBuilding[F] =
  pathBuildingFFI(
    _.addOval(
      SkRRect(
        where.x,
        where.y,
        rrect.rect.width,
        rrect.rect.height,
        rrect.radiiArray
      ),
      PathDirection.CLOCKWISE,
      1
    )
  )
end addRRect
