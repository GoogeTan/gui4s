package gui4s.android.skia

import cats.*
import cats.effect.*

import gui4s.core.layout.*
import gui4s.core.geometry.Rect

import org.jetbrains.skia.{Font, Paint, TextBlob}
import org.jetbrains.skia.shaper.Shaper

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

