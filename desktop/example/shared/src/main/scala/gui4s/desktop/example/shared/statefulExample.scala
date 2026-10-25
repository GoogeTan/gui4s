package gui4s.desktop.example.shared

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.TextWidget
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.kit.widgets.simpleStatefulWidget
import gui4s.desktop.skija.*

def statefulExample(
  text: TextWidget,
  typeface : Typeface,
)(using ClickCatcher) : DesktopWidget[Nothing] =
  simpleStatefulWidget(
    name = "state",
    initialState = 0,
    eventHandler = (state, events) => 
      if events.isEmpty then
        (state + events.length).some.pure
      else
        None.pure,
    body = state =>
      text(
        "test text " + state.toString,
        SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF8484A4))
      ).onClick(())
  )
end statefulExample
