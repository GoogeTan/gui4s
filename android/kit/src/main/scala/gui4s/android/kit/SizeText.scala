package gui4s.android.kit

import catnip.Cache
import gui4s.core.layout.{Sized, SizedC}
import gui4s.android.skia.{SkijaPlacedText, SkijaTextStyle, placeText}

import org.jetbrains.skia.shaper.Shaper

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]
type TextCache[IO[_]] = Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]

def sizeTextFFI[
  OuterPlace[_] : Sync
](
  getAvailablePlace : OuterPlace[Option[Float]],
  shaper: Shaper,
  cache : TextCache[OuterPlace],
) : SizeText[OuterPlace * SizedC[Float]] =
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