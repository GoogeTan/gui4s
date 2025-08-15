package me.katze.gui4s.example
package api.widget

import api.effects.{SkijaPlace, SkijaPlaceT}
import api.widget.SizeText

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.{Functor, Monad, Monoid}
import me.*
import me.katze.gui4s.skija.{SkijaDraw, SkijaPlacedText, SkijaTextStyle, drawText}
import me.katze.gui4s.widget.library.Widget

def skijaText[
  IO[_] : Monad,
  Update[_] : Monad,
  Place[_] : Functor,
  RecompositionReaction : Monoid as RRM,
  HandlableEvent,
](
  ffi : ForeighFunctionInterface[IO],
  textSizer : SizeText[Place],
  text : String,
  style : SkijaTextStyle,
) : Place[Widget[Update, Place, SkijaDraw[IO], RecompositionReaction, HandlableEvent]] =
  me.katze.gui4s.widget.library.text[
    Update,
    Place,
    SkijaDraw[IO],
    RecompositionReaction,
    HandlableEvent,
    SkijaPlacedText
  ](
    textSizer(text, style),
    drawText(ffi, _),
    RRM.empty,
  )
end skijaText

