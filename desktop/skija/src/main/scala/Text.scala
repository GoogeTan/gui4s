package gui4s.desktop.skija

import cats.effect.Sync
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint, TextBlob}

final case class SkijaTextStyle(font: Font, paint: Paint)
final case class SkijaPlacedText(original : String, textBlob: TextBlob, paint: Paint)

def placeText[F[_] : Sync as S](
                                  shaper : Shaper,
                                  text : String,
                                  style : SkijaTextStyle,
                                  maxWidth : Option[Float]
                                ) : F[Sized[Float, SkijaPlacedText]] =
  S.delay:
    val blob = maxWidth match
      case Some(value) => shaper.shape(text, style.font, value)
      case None => shaper.shape(text, style.font)
    val blobBounds = style.font.measureText(text, style.paint) // TODO границы из text blob кривые
    Sized(SkijaPlacedText(text, blob, style.paint), Rect(blobBounds.getWidth, blobBounds.getHeight))
end placeText

