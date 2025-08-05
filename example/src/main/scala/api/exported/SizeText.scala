package me.katze.gui4s.example
package api.exported

import catnip.ForeighFunctionInterface
import cats.data.StateT
import cats.{Monad, ~>}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}
import scalacache.Cache
import cats.syntax.all.*

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]

def sizeTextStateTFFI[IO[_] : Monad](
                                    ffi : ForeighFunctionInterface[IO],
                                    shaper: Shaper,
                                    cache : Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
                                  ) : SizeText[[Value] =>> StateT[IO, Bounds[Float], Sized[Float, Value]]] =
  (text: String, options: SkijaTextStyle) =>
    StateT(
      bounds =>
        cache.cachingF(
          (text, options, bounds.horizontal.max)
        )(None)(
          placeText(ffi = ffi,
            shaper = shaper,
            text = text,
            style = options,
            maxWidth = bounds.horizontal.max
          ).map(placedText => new Sized(placedText.text, placedText.width, placedText.height))
        ).map(placedText => (bounds, placedText))
    )
end sizeTextStateTFFI

def sizeTextLift[F[_], G[_]](original : SizeText[F], inj : F ~> G) : SizeText[G] =
  (text, options) =>
    inj(original(text, options))
end sizeTextLift
