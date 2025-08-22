package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.*
import cats.syntax.all.*
import cats.{Applicative, Monad, ~>}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.{Sized, SizedT}
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle, placeText}

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]
type TextCache[IO[_]] = Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]

def sizeTextFFI[
  OuterPlace[_] : Monad
](
  getAvailablePlace : OuterPlace[Option[Float]],
  ffi : ForeighFunctionInterface[OuterPlace],
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
          ).map(placedText => new Sized(placedText.text, placedText.width, placedText.height))
        )
end sizeTextFFI

trait Cache[IO[_], K, V]:
  def get(key : K) : IO[Option[V]]
    
  def getOrPut(key : K, value : IO[V]) : IO[V]
end Cache

final class ScalacacheCache[IO[_], K, V](cache :  scalacache.Cache[IO, K, V]) extends Cache[IO, K, V]:
  override def get(key: K): IO[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key : K, value : IO[V]) : IO[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache


final class MapKCache[IO[_] : Applicative, GIO[_] : Monad, K, V](original : Cache[IO, K, V], f : IO ~> GIO) extends Cache[GIO, K, V]:
  override def get(key : K) : GIO[Option[V]] =
    f(original.get(key))
  end get
    
  override def getOrPut(key : K, value : GIO[V]) : GIO[V] =
    f(original.get(key)).flatMap:
      case Some(res) => res.pure
      case None =>
        value.flatMap:
          res =>
            f(original.getOrPut(key, res.pure))
  end getOrPut
end MapKCache
