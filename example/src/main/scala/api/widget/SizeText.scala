package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Functor, Monad, ~>}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.geometry.{InfinityOr, Rect}
import me.katze.gui4s.layout.{Sized, SizedT}
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]
type Cache[IO[_], K, V] = (K, IO[V]) => IO[V]
type TextCache[IO[_]] = Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]

def sizeTextFFI[
  IO[_] : Functor, 
  OuterPlace[_] : Monad
](
  getAvailablePlace : OuterPlace[Option[Float]],
  ffi : ForeighFunctionInterface[IO],
  shaper: Shaper,
  cache : TextCache[IO],
  liftF : IO ~> OuterPlace
) : SizeText[OuterPlace * SizedT[Float]] =
  (text: String, options: SkijaTextStyle) =>
    getAvailablePlace.flatMap:
      bounds =>
        liftF(
          cache(
            (text, options, bounds),
            placeText(ffi = ffi,
              shaper = shaper,
              text = text,
              style = options,
              maxWidth = bounds
            ).map(placedText => new Sized(placedText.text, placedText.width, placedText.height))
          )
        )
end sizeTextFFI

def scalacacheCache[IO[_], K, V](cache :  scalacache.Cache[IO, K, V]) : Cache[IO, K, V] =
  (k, V) =>
    cache.cachingF(k)(None)(V)
end scalacacheCache
