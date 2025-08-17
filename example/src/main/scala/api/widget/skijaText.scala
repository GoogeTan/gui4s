package me.katze.gui4s.example
package api.widget

import api.effects.{SkijaPlace, SkijaPlaceT}
import api.widget.SizeText

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.{Functor, Monad, Monoid}
import io.github.humbleui.skija.shaper.Shaper
import me.*
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}
import me.katze.gui4s.widget.library.Widget

type TextWidget[Widget] = (text : String, style : SkijaTextStyle) => Widget

def skijaText[
  IO[_] : Monad,
  Update[_] : Monad,
  RecompositionReaction : Monoid as RRM,
  HandlableEvent,
  PlaceError,
](
  ffi : ForeighFunctionInterface[IO],
  shaper : Shaper,
  textCache : TextCache[IO],
) : TextWidget[SkijaPlace[IO, Float, PlaceError, Widget[Update, SkijaPlaceT[IO, Float, PlaceError], SkijaDraw[IO], RecompositionReaction, HandlableEvent]]] =
  (text : String, style : SkijaTextStyle) =>
    me.katze.gui4s.widget.library.text[
      Update,
      SkijaPlaceT[IO, Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      HandlableEvent,
      SkijaPlacedText
    ](
      SkijaPlace.sizeText(ffi, shaper, textCache)(text, style),
      drawText(ffi, _),
      RRM.empty,
    )
end skijaText

