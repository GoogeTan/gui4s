package me.katze.gui4s.example
package draw.skija

import cats.Functor
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, Paint, TextBlob}
import me.katze.gui4s.impure.FFI
import me.katze.gui4s.layout.{Measurable, Sized}
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}
import cats.syntax.all.*

def sizeText[F[+_] : Functor](ffi : FFI[F], text: String, shaper : Shaper, options: SkijaTextStyle):  Measurable[F, Float, SkijaPlacedText] =
  bounds =>
    placeText(ffi = ffi,
      shaper = shaper,
      text = text,
      style = options,
      maxWidth = bounds.horizontal.max
    ).map((placedText) => Sized(placedText.text, placedText.width, placedText.height))
end sizeText

