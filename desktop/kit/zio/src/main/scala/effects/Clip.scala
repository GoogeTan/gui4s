package gui4s.desktop.kit.zio
package effects

import catnip.ForeignFunctionInterface
import cats.Monad
import cats.kernel.Monoid
import gui4s.core.geometry.Point3d
import gui4s.desktop.skija.{SkijaDraw, clipToPath}
import io.github.humbleui.skija.{Path, PathFillMode, PathOp}
import effects.given
import zio.*
import zio.interop.catz.*

type Clip = gui4s.desktop.kit.common.effects.Clip

object Clip:
  given clipMonoid : Monoid[Clip] =
    gui4s.desktop.kit.common.effects.Clip.clipMonoid

  def moveClipToPoint(path : Clip, point : Point3d[Float]) : Clip =
    gui4s.desktop.kit.common.effects.Clip.moveClipToPoint(path, point)
  end moveClipToPoint

  def drawClipped(path: Clip, original: Draw): Draw =
    gui4s.desktop.kit.common.effects.Clip.drawClipped(path, original)
  end drawClipped
end Clip
