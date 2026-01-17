package gui4s.desktop

import cats.kernel.Monoid
import io.github.humbleui.skija.{PathFillMode, PathOp}

package object skija {
  type Canvas = io.github.humbleui.skija.Canvas
  type Paint = io.github.humbleui.skija.Paint
  type Color = io.github.humbleui.skija.Color
  type Font = io.github.humbleui.skija.Font
  type FontStyle = io.github.humbleui.skija.FontStyle
  type FontMgr = io.github.humbleui.skija.FontMgr
  type Typeface = io.github.humbleui.skija.Typeface
  type Rect = io.github.humbleui.types.Rect
  type RRect = io.github.humbleui.types.RRect
  type Point = io.github.humbleui.types.Point
  type Path = io.github.humbleui.skija.Path
  type Image = io.github.humbleui.skija.Image

  given clipMonoid : Monoid[Path] with
    override def empty: Path =
      new io.github.humbleui.skija.Path().setFillMode(PathFillMode.INVERSE_WINDING)
    end empty

    override def combine(x: Path, y: Path): Path =
      io.github.humbleui.skija.Path.makeCombining(x, y, PathOp.INTERSECT)
    end combine
  end clipMonoid
}