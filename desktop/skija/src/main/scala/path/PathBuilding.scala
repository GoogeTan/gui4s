package gui4s.desktop.skija
package path

import cats.data.Kleisli
import cats.effect.Sync
import gui4s.core.geometry.{Point2d, RRect}
import io.github.humbleui.skija.Path
import io.github.humbleui.types.{RRect as SkRRect, Rect as SkRect}

type PathBuilding[F[_]] = Kleisli[F, Path, Path]

def pathBuildingFFI[F[_] : Sync as S](f : Path => Path) : PathBuilding[F] =
  Kleisli(path => S.delay(f(path)))
end pathBuildingFFI

def addOval[F[_] : Sync](x : Float, y : Float, width : Float, height : Float) : PathBuilding[F] =
  pathBuildingFFI(_.addOval(SkRect.makeXYWH(x, y, width, height)))
end addOval

def addRRect[F[_] : Sync](where : Point2d[Float], rrect : RRect[Float]) : PathBuilding[F] =
  pathBuildingFFI(
    _.addOval(
      SkRRect.makeComplexXYWH(
        where.x,
        where.y,
        rrect.rect.width,
        rrect.rect.height,
        rrect.radiiArray
      )
    )
  )
end addRRect
