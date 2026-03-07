package gui4s.desktop.example

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.kit.widgets.{DesktopWidget, TextWidget, statefulWidget}
import gui4s.desktop.skija.*

def statefulExample(
  text: TextWidget,
  onClick : ClickCatcher,
  typeface : Typeface,
) : DesktopWidget[Nothing] =
  statefulWidget(
    name = "state",
    initialState = 0,
    eventHandler = (state, _, _) => (state + 1).pure,
    body = state =>
      onClick(())(
        text(
          "test text " + state.toString,
          SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF8484A4))
        )
      )
  )
end statefulExample
