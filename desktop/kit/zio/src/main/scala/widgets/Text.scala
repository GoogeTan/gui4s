package gui4s.desktop.kit.zio
package widgets

import gui4s.desktop.kit.common.widgets.TextWidget
import gui4s.desktop.kit.common.*
import io.github.humbleui.skija.shaper.Shaper
import zio.*
import zio.interop.catz.*

def text[Event](
  shaper : Shaper,
  textCache : TextCache[Task],
) : TextWidget[Task, Event] =
  gui4s.desktop.kit.common.widgets.text(shaper, textCache)
end text

