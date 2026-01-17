package gui4s.desktop.example

import catnip.syntax.all.given
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.*
import glfw4s.core.pure.*
import glfw4s.jna.bindings.types.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.kit.widgets.{DesktopWidget, statefulWidget, TextWidget}
import gui4s.desktop.skija.*
import gui4s.desktop.skija.typeface.*
import io.github.humbleui.skija.*

object StatefulExample extends UIApp:
  override val settings = WindowCreationSettings(
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
