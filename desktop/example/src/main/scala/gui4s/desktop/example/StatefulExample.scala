package gui4s.desktop.example

import catnip.syntax.all.given
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.*
import glfw4s.core.pure.PostInit
import glfw4s.jna.bindings.types.*
import gui4s.core.geometry.*
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.kit.widgets.{DesktopWidget, statefulWidget, text}
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
                      glfw: PostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow],
                      window: GLFWwindow,
                      eventBus: Queue[IO, DownEvent],
                    ) : Resource[AppIO, DesktopWidget[AppIO, ApplicationRequest]] =
    for
      shaper <- createShaper[AppIO]
      cache : TextCache[AppIO] <- ScalacacheCache()
      textWidget = text(shaper, cache)
      typeface <- defaultTypeface[AppIO]
      clickSource <- clickEventSource[AppIO, IO, GLFWmonitor, GLFWwindow](window, glfw, eventBus).eval
      onClick = [Event] => (event : Event) => clickCatcher(glfw.cursorPos(window).map(Point2d(_, _)), event, clickSource)
    yield statefulWidget[AppIO][Int, ApplicationRequest, Unit](
      name = "state",
      initialState = 0,
      eventHandler = (state, _, _) => (state + 1).pure[UpdateC[AppIO, ApplicationRequest]],
      body = state =>
        onClick(())(
          textWidget(
            "test text " + state.toString,
            SkijaTextStyle(new Font(typeface, 24), new Paint().setColor(0xFF8484A4))
          )
        )
    )
  end main
end StatefulExample
