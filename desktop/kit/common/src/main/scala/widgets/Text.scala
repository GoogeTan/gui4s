package gui4s.desktop.kit
package widgets

import effects.*
import effects.Place.given
import effects.Update.given

import catnip.effect.SyncForeignFunctionInterface
import cats.effect.IO
import gui4s.decktop.widget.library.text as genericText
import gui4s.desktop.skija.*
import io.github.humbleui.skija.shaper.Shaper

def text[IO[_], Event](
  shaper : Shaper,
  textCache : TextCache[IO],
  ffi : SyncForeignFunctionInterface[IO]
)(
  text : String,
  style : SkijaTextStyle
) : DesktopWidget[IO, Event] =
  genericText[
    UpdateC[IO, Event],
    PlaceC[IO],
    SkijaDraw[IO],
    RecompositionReaction[IO],
    DownEvent,
    SkijaPlacedText
  ](
    Place.sizeText(ffi, shaper, textCache)(text, style),
    drawText(ffi, _),
    RecompositionReaction.empty,
  )
end text

