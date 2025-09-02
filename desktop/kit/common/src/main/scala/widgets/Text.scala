package gui4s.desktop.kit
package common.widgets

import common.effects.*
import common.*
import common.effects.Place.given

import catnip.ForeignFunctionInterface
import cats.*
import gui4s.desktop.skija.*
import gui4s.desktop.widget.library.text as genericText
import io.github.humbleui.skija.shaper.Shaper

type TextWidget[IO[_], Event] =(
  text : String,
  style : SkijaTextStyle
) => DesktopWidget[IO, Event]

def text[IO[_] : {Monad, ForeignFunctionInterface as ffi}, Event](
  shaper : Shaper,
  textCache : TextCache[IO],
) : TextWidget[IO, Event] = 
  (text : String, style : SkijaTextStyle) =>
    genericText[
      UpdateC[IO, Event],
      PlaceC[IO],
      SkijaDraw[IO],
      RecompositionReaction[IO],
      DownEvent,
      SkijaPlacedText
    ](
      Place.sizeText(shaper, textCache)(text, style),
      drawText(ffi, _),
      RecompositionReaction.empty,
    )
end text

