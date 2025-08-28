package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.Place.given
import effects.Update.given
import effects.Draw.given

import catnip.effect.SyncForeignFunctionInterface
import gui4s.decktop.widget.library.text as genericText
import gui4s.desktop.skija.*
import io.github.humbleui.skija.shaper.Shaper
import zio.*

def text[Event](shaper : Shaper, textCache : TextCache[Task])(text : String, style : SkijaTextStyle) : DesktopWidget[Event] =
  genericText[
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
    DownEvent,
    SkijaPlacedText
  ](
    Place.sizeText(shaper, textCache)(text, style),
    Draw.drawText,
    RecompositionReaction.empty,
  )
end text

