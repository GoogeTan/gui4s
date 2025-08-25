package gui4s.desktop.skija

import catnip.ForeignFunctionInterface
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint, TextBlob}

final case class SkijaTextStyle(font: Font, paint: Paint)
final case class SkijaPlacedText(original : String, textBlob: TextBlob, paint: Paint)

def placeText[F[_]](
                     ffi : ForeignFunctionInterface[F],
                     shaper : Shaper,
                     text : String,
                     style : SkijaTextStyle,
                     maxWidth : Option[Float]
                    ) : F[(text : SkijaPlacedText, width : Float, height : Float)] =
  ffi.delay:
    val blob = maxWidth match
      case Some(value) => shaper.shape(text, style.font, value.toFloat)
      case None => shaper.shape(text, style.font)
    val blobBounds = style.font.measureText(text, style.paint) // границы из text blob кривые
    (SkijaPlacedText(text, blob, style.paint), blobBounds.getWidth, blobBounds.getHeight)
end placeText

