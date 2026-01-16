package gui4s.android.kit.effects

import gui4s.core.geometry.Point3d
import gui4s.android.kit.effects.Draw.given
import gui4s.android.skia.canvas.withClipedPath

import org.jetbrains.skia.{Path, PathFillMode, PathOp}

type Clip = Path

object Clip:
  // TODO Исправить этот ужас, ибо это потенциальная утечка памяти
  given clipMonoid : Monoid[Clip] with
    override def empty: Clip =
      val path = new Path()
      path.setFillMode(PathFillMode.INVERSE_WINDING)
      path
    end empty

    override def combine(x: Clip, y: Clip): Clip =
      Path.Companion.makeCombining(x, y, PathOp.INTERSECT)
    end combine
  end clipMonoid

  def moveClipToPoint(path : Clip, point : Point3d[Float]) : Clip =
    clipMonoid.empty.offset(
      point.x,
      point.y,
      path,
    )
  end moveClipToPoint

  def drawClipped[IO[_] : Sync](path: Clip, original: Draw[IO]): Draw[IO] =
    withClipedPath(path, original)
  end drawClipped
end Clip
