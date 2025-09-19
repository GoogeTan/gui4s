package gui4s.desktop.skija
package path

import catnip.ForeignFunctionInterface
import cats.data.Kleisli
import gui4s.core.geometry.{Point2d, RRect}
import io.github.humbleui.skija.Path
import io.github.humbleui.types.{Rect as SkRect, RRect as SkRRect}

type PathBuilding[F[_]] = Kleisli[F, Path, Path]

def pathBuildingFFI[F[_] : ForeignFunctionInterface as ffi](f : Path => Path) : PathBuilding[F] =
  Kleisli(path => ffi(f(path)))
end pathBuildingFFI

def addOval[F[_] : ForeignFunctionInterface as ffi](x : Float, y : Float, width : Float, height : Float) : PathBuilding[F] =
  pathBuildingFFI(_.addOval(SkRect.makeXYWH(x, y, width, height)))
end addOval

def addRRect[F[_] : ForeignFunctionInterface as ffi](where : Point2d[Float], rrect : RRect[Float]) : PathBuilding[F] =
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
