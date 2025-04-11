package me.katze.gui4s.example
package draw.skija

import api.impl.LayoutPlacementMeta

import me.katze.gui4s.layout.{Measurable, Sized}
import draw.TextStyle

import cats.data.ReaderT
import io.github.humbleui.skija.{Font, Paint, TextBlob}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.widget.library.TextPlacement

final case class SkijaTextStyle(font: Font, paint: Paint)
final case class SkijaPlacedText(textBlob: TextBlob, paint: Paint)

given skijaTextPlacement[F[_] : Impure as I]: TextPlacement[Shaper, SkijaTextStyle, Measurable[F, Float, SkijaPlacedText]] with
  override def sizeText(text: String, shaper : Shaper, options: SkijaTextStyle):  Measurable[F, Float, SkijaPlacedText] =
    bounds =>
      I:
        val blob = bounds.horizontal.max match
          case Some(value) => shaper.shape(text, options.font, value)
          case None => shaper.shape(text, options.font)
        val blobBounds = blob.getBounds  
        Sized(SkijaPlacedText(blob, options.paint), blobBounds.getRight, blobBounds.getBottom)
  end sizeText
end skijaTextPlacement

