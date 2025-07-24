package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint, TextBlob}

final case class SkijaTextStyle(font: Font, paint: Paint)
final case class SkijaPlacedText(original : String, textBlob: TextBlob, paint: Paint)

def placeText[F[_]](
                      ffi : ForeighFunctionInterface[F],
                      shaper : Shaper,
                      text : String,
                      style : SkijaTextStyle,
                      maxWidth : Option[Pixel]
                    ) : F[(text : SkijaPlacedText, width : Pixel, height : Pixel)] =
  ffi.delay:
    val blob = maxWidth match
      case Some(value) => shaper.shape(text, style.font, value.toFloat)
      case None => shaper.shape(text, style.font)
    val blobBounds = style.font.measureText(text, style.paint) // границы из text blob кривые
    (SkijaPlacedText(text, blob, style.paint), Pixel(blobBounds.getWidth), Pixel(blobBounds.getHeight))
end placeText

