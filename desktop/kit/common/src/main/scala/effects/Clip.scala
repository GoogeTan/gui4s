package gui4s.desktop.kit
package effects

import catnip.ForeignFunctionInterface
import cats.Monad
import cats.kernel.Monoid
import io.github.humbleui.skija.{Path, PathFillMode, PathOp}
import gui4s.core.geometry.Point3d
import gui4s.desktop.skija.{SkijaDraw, clipToPath}

type Clip = Path

object Clip:
  given clipMonoid : Monoid[Clip] with
    override def empty: Clip =
      new Path().setFillMode(PathFillMode.INVERSE_WINDING)
    end empty

    override def combine(x: Clip, y: Clip): Clip =
      Path.makeCombining(x, y, PathOp.INTERSECT)
    end combine
  end clipMonoid

  def moveClipToPoint(path : Clip, point : Point3d[Float]) : Clip =
    clipMonoid.empty.addPath(
      path,
      point.x,
      point.y
    )
  end moveClipToPoint

  def drawClipped[IO[_] : Monad](ffi: ForeignFunctionInterface[IO])(path: Clip, original: SkijaDraw[IO]): SkijaDraw[IO] =
    clipToPath(ffi, path, original)
  end drawClipped
end Clip
