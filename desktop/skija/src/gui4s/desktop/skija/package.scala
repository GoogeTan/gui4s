package gui4s.desktop

import cats.kernel.Monoid
import io.github.humbleui.skija.PathFillMode
import io.github.humbleui.skija.PathOp

package object skija:
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
  type Shader = io.github.humbleui.skija.Shader
  type Data = io.github.humbleui.skija.Data
  type BlendMode = io.github.humbleui.skija.BlendMode
  type PaintMode = io.github.humbleui.skija.PaintMode
  type IRange = io.github.humbleui.types.IRange
  type IRect = io.github.humbleui.types.IRect
  type Surface = io.github.humbleui.skija.Surface

  // Java static members are not accessible through type aliases, so they are forwarded here.
  object Rect:
    def makeWH(width : Float, height : Float) : Rect = io.github.humbleui.types.Rect.makeWH(width, height)
    def makeLTRB(left : Float, top : Float, right : Float, bottom : Float) : Rect =
      io.github.humbleui.types.Rect.makeLTRB(left, top, right, bottom)
  end Rect

  object RRect:
    def makeLTRB(left : Float, top : Float, right : Float, bottom : Float, radius : Float) : RRect =
      io.github.humbleui.types.RRect.makeLTRB(left, top, right, bottom, radius)
  end RRect

  object IRect:
    def makeXYWH(x : Int, y : Int, width : Int, height : Int) : IRect =
      io.github.humbleui.types.IRect.makeXYWH(x, y, width, height)
  end IRect

  object Path:
    def makeOval(rect : Rect) : Path = io.github.humbleui.skija.Path.makeOval(rect)
    def makeRRect(rect : RRect) : Path = io.github.humbleui.skija.Path.makeRRect(rect)
    def makeRect(rect : Rect) : Path = io.github.humbleui.skija.Path.makeRect(rect)
  end Path

  object FontMgr:
    def getDefault : FontMgr = io.github.humbleui.skija.FontMgr.getDefault
  end FontMgr

  object Image:
    def makeDeferredFromEncodedBytes(bytes : Array[Byte]) : Image =
      io.github.humbleui.skija.Image.makeDeferredFromEncodedBytes(bytes)
  end Image

  object BlendMode:
    val DIFFERENCE : BlendMode = io.github.humbleui.skija.BlendMode.DIFFERENCE
  end BlendMode

  object FontStyle:
    val BOLD_ITALIC : FontStyle = io.github.humbleui.skija.FontStyle.BOLD_ITALIC
  end FontStyle

  object Surface:
    def makeRasterN32Premul(width : Int, height : Int) : Surface =
      io.github.humbleui.skija.Surface.makeRasterN32Premul(width, height)
  end Surface

  object PaintMode:
    val STROKE : PaintMode = io.github.humbleui.skija.PaintMode.STROKE
  end PaintMode

  given clipMonoid : Monoid[Path] with
    override def empty: Path =
      new io.github.humbleui.skija.Path().setFillMode(PathFillMode.INVERSE_WINDING)
    end empty

    override def combine(x: Path, y: Path): Path =
      io.github.humbleui.skija.Path.makeCombining(x, y, PathOp.INTERSECT)
    end combine
  end clipMonoid
end skija