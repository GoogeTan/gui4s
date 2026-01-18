package gui4s.desktop.kit
package effects

import cats.effect.kernel.Sync

import gui4s.core.geometry.Point3d

import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.skija._
import gui4s.desktop.skija.canvas.withClipedPath

type Clip = Path

object Clip:
  export gui4s.desktop.skija.clipMonoid

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
