package gui4s.desktop.kit
package common.effects

import Draw.given
import cats.effect.kernel.Sync
import cats.kernel.Monoid
import gui4s.core.geometry.Point3d
import gui4s.desktop.skija.canvas.withClipedPath
import io.github.humbleui.skija.{Path, PathFillMode, PathOp}

type Clip = Path

object Clip:
  // TODO Исправить этот ужас, ибо это потенциальная утечка памяти
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

  def drawClipped[IO[_] : Sync](path: Clip, original: Draw[IO]): Draw[IO] =
    withClipedPath(path, original)
  end drawClipped
end Clip
