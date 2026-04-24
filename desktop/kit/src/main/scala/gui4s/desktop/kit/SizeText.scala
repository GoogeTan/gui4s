package gui4s.desktop.kit

import catnip.Cache
import catnip.syntax.all.*
import cats.effect.kernel.Sync
import cats.syntax.all.*

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.core.layout.SizedC

import gui4s.desktop.skija.SkijaPlacedText
import gui4s.desktop.skija.SkijaTextStyle
import gui4s.desktop.skija.placeText
import gui4s.desktop.skija.shaper.*

type SizeText[Place[_]] = (text: String, options: SkijaTextStyle) => Place[SkijaPlacedText]
type TextCache[IO[_]] = Cache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Rect[Float], SkijaPlacedText]]

def sizeTextFFI[
  PlacementEffect[_] : Sync
](
  getAvailablePlace : PlacementEffect[Option[Float]],
  shaper: Shaper,
  cache : TextCache[PlacementEffect],
) : SizeText[PlacementEffect * SizedC[Rect[Float]]] =
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