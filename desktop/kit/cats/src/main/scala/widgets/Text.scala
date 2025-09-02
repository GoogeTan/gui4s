package gui4s.desktop.kit.cats
package widgets

import effects.Place.given
import effects.Update.given
import effects.{*, given}

import gui4s.desktop.skija.SkijaTextStyle
import gui4s.desktop.kit.common.*
import io.github.humbleui.skija.shaper.Shaper
import _root_.cats.effect.IO

def text[Event](
  shaper : Shaper,
  textCache : TextCache[IO],
)(
  text : String,
  style : SkijaTextStyle
) : DesktopWidget[Event] =
  gui4s.desktop.kit.common.widgets.text(shaper, textCache)(text, style)
end text

