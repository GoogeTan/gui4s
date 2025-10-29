package gui4s.desktop.kit

import catnip.Cache
import catnip.syntax.all.*
import cats.effect.kernel.Sync
import cats.syntax.all.*
import gui4s.core.layout.{Sized, SizedT}
import gui4s.desktop.skija.{SkijaPlacedText, SkijaTextStyle, placeText}
import io.github.humbleui.skija.shaper.Shaper

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]
type TextCache[IO[_]] = Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]

def sizeTextFFI[
  OuterPlace[_] : Sync
](
  getAvailablePlace : OuterPlace[Option[Float]],
  shaper: Shaper,
  cache : TextCache[OuterPlace],
) : SizeText[OuterPlace * SizedT[Float]] =
  (text: String, options: SkijaTextStyle) =>
    getAvailablePlace.flatMap:
      bounds =>
        cache.getOrPut(
          (text, options, bounds),
          placeText(
            shaper = shaper,
            text = text,
            style = options,
            maxWidth = bounds
          )
        )
end sizeTextFFI