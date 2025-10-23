package gui4s.desktop.kit.cats
package widgets

import cats.data.EitherT
import gui4s.desktop.skija.SkijaTextStyle
import gui4s.desktop.kit.common.*
import io.github.humbleui.skija.shaper.Shaper
import _root_.cats.effect.IO
import glfw4s.core.types.GlfwError

def text[Event](
   shaper : Shaper,
   textCache : TextCache[EitherT[IO, GlfwError, *]],
)(
  text : String,
  style : SkijaTextStyle
) : DesktopWidget[Event] =
  gui4s.desktop.kit.common.widgets.text[EitherT[IO, GlfwError, *], Event](shaper, textCache)(text, style)
end text

