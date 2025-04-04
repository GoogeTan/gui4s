package me.katze.gui4s.skija

import cats.effect.Resource
import io.github.humbleui.skija.FontStyle
import io.github.humbleui.skija.shaper.ShapingOptions
import io.github.humbleui.types.Point

trait Skija[F[_]]:
  type Typeface

  extension(value : Typeface)
    def bold : Boolean
    def italic :Boolean
    def fixedPitch : Boolean
  end extension

  def makTypefaceFromFile(name : String, style: FontStyle*) : Resource[F, Typeface]
  val defaultTypeface : Resource[F, Typeface]

  type Font

  final lazy val defaultFont : Resource[F, Font] = font(None)

  def font(typeface: Option[Typeface]) : Resource[F, Font]
  def font(typeface: Option[Typeface], size : Float) : Resource[F, Font]
  def font(typeface: Option[Typeface], size : Float, scaleX : Float, skewX : Float) : Resource[F, Font]

  type Shaper
  type TextBlob
  type TextLine

  extension (value : Shaper)
    def shape(
                text: String,
                font: Font,
                options : ShapingOptions = ShapingOptions.DEFAULT,
                width : Float = Float.PositiveInfinity,
                point : Point = Point.ZERO,
              ): F[TextBlob]

    def shapeLine(
                    text : String,
                    font: Font,
                    options: ShapingOptions = ShapingOptions.DEFAULT
                  ) : F[TextLine]
  end extension

  val createShaper : Resource[F, Shaper]

  type Canvas
  type CanvasSavePoint

  type Paint
  def defaltPaint : Resource[F, Paint]

  def saveState(canvas : Canvas) : F[CanvasSavePoint]
  def loadState(canvas: Canvas, canvasSavePoint: CanvasSavePoint) : F[Unit]
  def translate(canvas : Canvas) : F[Unit]
  def drawTextBlob(canvas: Canvas, x : Float, y : Float, paint : Paint) : F[Unit]

end Skija
