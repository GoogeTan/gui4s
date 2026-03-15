package gui4s.desktop.example

import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._

import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.TextWidget
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.kit.widgets.statefulWidget
import gui4s.desktop.skija._

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
