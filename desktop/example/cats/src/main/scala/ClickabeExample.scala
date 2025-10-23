package gui4s.desktop.example.cats

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.*
import gui4s.desktop.kit.common.*
import gui4s.desktop.kit.common.widgets.decorator.*
import gui4s.desktop.kit.common.effects.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.*
import gui4s.desktop.skija.*
import glfw4s.core.*
import glfw4s.jvm.types.*
import gui4s.core.geometry.*
import glfw4s.core.types.GlfwError
import gui4s.desktop.kit.common.widgets.{DesktopWidget, TextWidget, statefulWidget, text}

object ClickabeExample extends CatsApp:
  final case class PreInit(shaper : Shaper, globalTextCache : TextCache[EitherT[IO, GlfwError, *]], mousePosition : EitherT[IO, GlfwError, Point2d[Float]])

  def preInit(backend : SkijaBackend[EitherT[IO, GlfwError, *], Resource[EitherT[IO, GlfwError, *], *],  IO, GLFWmonitor, GLFWwindow, DownEvent]) : Resource[EitherT[IO, GlfwError, *], PreInit] =
    for
      shaper <- createShaper[EitherT[IO, GlfwError, *]]
      cache : TextCache[EitherT[IO, GlfwError, *]] <- ScalacacheCache()
    yield PreInit(shaper, cache, backend.mousePosition)
  end preInit

  val settings = WindowCreationSettings(
    title = "Gui4s clickable example",
    width = 620,
    height = 480,
  )

  def main(preInit : PreInit) : DesktopWidget[CatsIO, ApplicationRequest] =
    extension[Event](value : DesktopWidget[CatsIO, Event])
      def onClick(event : Event) : DesktopWidget[CatsIO, Event] =
        clickCatcher(preInit.mousePosition, event)(value)
      end onClick
    end extension

    def textWidget[Event] : TextWidget[CatsIO, Event] =
      text(
        preInit.shaper,
        preInit.globalTextCache
      )
    end textWidget

    statefulWidget[EitherT[IO, GlfwError, *]][Int, ApplicationRequest, Unit](
      name = "state",
      initialState = 0,
      eventHandler = (state, _, _) => (state + 1).pure[UpdateC[CatsIO, ApplicationRequest]],
      body = state =>
        textWidget(
          "test text " + state.toString,
          SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
        ).onClick(())
    )
  end main
end ClickabeExample
