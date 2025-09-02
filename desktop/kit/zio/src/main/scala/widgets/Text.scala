package gui4s.desktop.kit.zio
package widgets

import effects.{*, given}

import gui4s.desktop.kit.widgets.TextWidget
import io.github.humbleui.skija.shaper.Shaper
import zio.*
import zio.interop.catz.*

def text[Event](
  shaper : Shaper,
  textCache : TextCache[Task],
) : TextWidget[Task, Event] =
  gui4s.desktop.kit.widgets.text(shaper, textCache)
end text

