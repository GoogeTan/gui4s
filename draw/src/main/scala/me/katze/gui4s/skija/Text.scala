package me.katze.gui4s.skija

import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint, TextBlob}
import me.katze.gui4s.impure.FFI

final case class SkijaTextStyle(font: Font, paint: Paint)
final case class SkijaPlacedText(textBlob: TextBlob, paint: Paint)

def placeText[F[_]](
                      ffi : FFI[F],
                      shaper : Shaper,
                      text : String, 
                      style : SkijaTextStyle, 
                      maxWidth : Option[Float]
                    ) : F[(text : SkijaPlacedText, width : Float, height : Float)] =
  ffi.delay:
    val blob = maxWidth match
      case Some(value) => shaper.shape(text, style.font, value)
      case None => shaper.shape(text, style.font)
    val blobBounds = blob.getBounds
    (SkijaPlacedText(blob, style.paint), blobBounds.getRight, blobBounds.getBottom)
end placeText

