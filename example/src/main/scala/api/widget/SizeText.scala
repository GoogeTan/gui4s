package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad, ~>}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}
import scalacache.Cache

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]

def sizeTextFFI[
  IO[_] : Functor, 
  Place[_] : Monad
](
  getBounds : Place[Bounds[Float]],
  ffi : ForeighFunctionInterface[IO],
  shaper: Shaper,
  cache : Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]],
  f : IO ~> Place 
) : SizeText[Place * Sized[Float, *]] =
  (text: String, options: SkijaTextStyle) =>
    getBounds.flatMap:
      bounds =>
        f(
          cache.cachingF(
            (text, options, bounds.horizontal.maximumLimit)
          )(None)(
            placeText(ffi = ffi,
              shaper = shaper,
              text = text,
              style = options,
              maxWidth = bounds.horizontal.maximumLimit
            ).map(placedText => new Sized(placedText.text, placedText.width, placedText.height))
          )
        )
end sizeTextFFI