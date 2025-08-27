package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.Place.given
import effects.Update.given

import catnip.effect.SyncForeignFunctionInterface
import cats.effect.IO
import gui4s.decktop.widget.library.text as genericText
import gui4s.desktop.skija.*
import io.github.humbleui.skija.shaper.Shaper

def text[Event](shaper : Shaper, textCache : TextCache[IO])(text : String, style : SkijaTextStyle) : DesktopWidget[Event] =
  val ffi = SyncForeignFunctionInterface[IO]()
  genericText[
    UpdateC[Event],
    Place,
    SkijaDraw[IO],
    RecompositionReaction,
    DownEvent,
    SkijaPlacedText
  ](
    Place.sizeText(ffi, shaper, textCache)(text, style),
    drawText(ffi, _),
    RecompositionReaction.empty,
  )
end text

