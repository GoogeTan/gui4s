package gui4s.desktop.kit.cats

import catnip.syntax.all.*
import catnip.{Cache, ForeignFunctionInterface}
import cats.Monad
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import gui4s.core.layout.{Sized, SizedT}
import gui4s.desktop.skija.{SkijaPlacedText, SkijaTextStyle, placeText}

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]
type TextCache[IO[_]] = Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]

def sizeTextFFI[
  OuterPlace[_] : Monad
](
  getAvailablePlace : OuterPlace[Option[Float]],
  ffi : ForeignFunctionInterface[OuterPlace],
  shaper: Shaper,
  cache : TextCache[OuterPlace],
) : SizeText[OuterPlace * SizedT[Float]] =
  (text: String, options: SkijaTextStyle) =>
    getAvailablePlace.flatMap:
      bounds =>
        cache.getOrPut(
          (text, options, bounds),
          placeText(ffi = ffi,
            shaper = shaper,
            text = text,
            style = options,
            maxWidth = bounds
          )
        )
end sizeTextFFI