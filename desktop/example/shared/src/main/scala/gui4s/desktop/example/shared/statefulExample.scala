package gui4s.desktop.example.shared

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.kit.widgets.{DesktopWidget, TextWidget, statefulWidget}
import gui4s.desktop.skija.*

def statefulExample(
  text: TextWidget,
  typeface : Typeface,
)(using ClickCatcher) : DesktopWidget[Nothing] =
  statefulWidget(
    name = "state",
    initialState = 0,
    eventHandler = (state, events) => (state + events.length).pure,
    body = state =>
      text(
        "test text " + state.toString,
        SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF8484A4))
      ).onClick(())
  )
end statefulExample
