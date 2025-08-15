package me.katze.gui4s.example
package api.effects

import catnip.ForeighFunctionInterface
import cats.Monad
import cats.kernel.Monoid
import io.github.humbleui.skija.{Path, PathFillMode, PathOp}
import me.katze.gui4s.geometry.{Point3d, Rect}
import io.github.humbleui.skija.Path
import io.github.humbleui.types.{RRect as SkijaRounedRect, Rect as SkijaRect}
import me.katze.gui4s.skija.SkijaDraw

opaque type SkijaClip = Path

object SkijaClip:
  given clipMonoid : Monoid[SkijaClip] with
    override def empty: SkijaClip =
      new Path().setFillMode(PathFillMode.INVERSE_WINDING)
    end empty

    override def combine(x: SkijaClip, y: SkijaClip): SkijaClip =
      Path.makeCombining(x, y, PathOp.INTERSECT)
    end combine
  end clipMonoid

  def skijaPathAt(path : SkijaClip, point : Point3d[Float]) : SkijaClip =
    clipMonoid.empty.addPath(
      path,
      point.x,
      point.y
    )
  end skijaPathAt

  def clipToPath[IO[_] : Monad](ffi: ForeighFunctionInterface[IO], path: SkijaClip, original: SkijaDraw[IO]): SkijaDraw[IO] =
    me.katze.gui4s.skija.clipToPath(ffi, path, original)
  end clipToPath  

  object Shapes:
    def round(rect : Rect[Float]) : SkijaClip =
      new Path().addOval(SkijaRect(0f, 0f, rect.width, rect.height))
    end round

    def roundedCorners(rect: Rect[Float], radius: Float): SkijaClip =
      new Path().addRRect(SkijaRounedRect.makeLTRB(0f, 0f, rect.width, rect.height, radius))
    end roundedCorners
  end Shapes
end SkijaClip
