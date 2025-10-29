package gui4s.desktop.example

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
import gui4s.desktop.kit.widgets.{DesktopWidget, TextWidget, statefulWidget, text}
import gui4s.desktop.skija.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.*

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
    extension[Event](value : DesktopWidget[AppIO, Event])
      def onClick(event : Event) : DesktopWidget[AppIO, Event] =
        clickCatcher(glfw.cursorPos(window).map(Point2d(_, _)), event)(value)
      end onClick
    end extension

    for
      shaper <- createShaper[AppIO]
      cache : TextCache[AppIO] <- ScalacacheCache()
      textWidget = text(shaper, cache)
    yield statefulWidget[AppIO][Int, ApplicationRequest, Unit](
      name = "state",
      initialState = 0,
      eventHandler = (state, _, _) => (state + 1).pure[UpdateC[AppIO, ApplicationRequest]],
      body = state =>
        textWidget(
          "test text " + state.toString,
          SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
        ).onClick(())
    )
  end main
end StatefulExample
