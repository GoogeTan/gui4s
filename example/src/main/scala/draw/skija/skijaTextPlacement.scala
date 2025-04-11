package me.katze.gui4s.example
package draw.skija

import api.impl.LayoutPlacementMeta

import me.katze.gui4s.layout.{Measurable, Sized}
import draw.TextStyle

import cats.data.ReaderT
import io.github.humbleui.skija.{Font, TextBlob}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.widget.library.TextPlacement

given skijaTextPlacement[F[_] : Impure as I]: TextPlacement[Shaper, Font, Measurable[F, Float, TextBlob]] with
  override def sizeText(text: String, shaper : Shaper, font: Font):  Measurable[F, Float, TextBlob] =
    bounds =>
      I:
        val blob = bounds.horizontal.max match
          case Some(value) => shaper.shape(text, font, value)
          case None => shaper.shape(text, font)
        val blobBounds = blob.getBounds  
        Sized(blob, blobBounds.getRight, blobBounds.getBottom)
  end sizeText
end skijaTextPlacement

