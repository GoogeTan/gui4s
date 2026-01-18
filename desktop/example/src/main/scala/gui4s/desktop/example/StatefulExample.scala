package gui4s.desktop.example

import catnip.syntax.all.given
import cats._
import cats.data._
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import glfw4s.core._
import glfw4s.core.pure._
import glfw4s.jna.bindings.types._

import gui4s.core.geometry._

import gui4s.desktop.kit._
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.TextWidget
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.kit.widgets.statefulWidget
import gui4s.desktop.skija._
import gui4s.desktop.skija.typeface._

object StatefulExample extends UIApp:
  override val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s clickable example",
    width = 620,
    height = 480,
  )

  override def main(
                     glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                     window: GLFWwindow,
                     eventBus: Queue[IO, DownEvent],
                   ) : Resource[AppIO, DesktopWidget[AppIO, Nothing]] =
    for
      shaper <- createShaper[AppIO]
      cache : TextCache[AppIO] <- ScalacacheCache()
      text = TextWidget(shaper, cache)
      typeface <- defaultTypeface[AppIO]
      clickSource <- clickEventSource[AppIO, IO, GLFWmonitor, GLFWwindow, GLFWcursor, Int](window, glfw, eventBus).eval
      onClick = [Event] => (event : Event) =>
        clickCatcher(glfw.getCursorPos(window).map((x, y) => Point2d(x.toFloat, y.toFloat)), event, clickSource)
    yield statefulWidget[AppIO][Int, Nothing, Unit](
      name = "state",
      initialState = 0,
      eventHandler = (state, _, _) => (state + 1).pure[UpdateC[AppIO, Nothing]],
      body = state =>
        onClick(())(
          text(
            "test text " + state.toString,
            SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF8484A4))
          )
        )
    )
  end main
end StatefulExample
