package gui4s.desktop.example

import cats.data.EitherT
import cats.effect.*
import cats.effect.std.Queue
import glfw4s.core.*
import glfw4s.core.pure.PostInit
import glfw4s.jna.bindings.types.*
import gui4s.core.geometry.*
import gui4s.core.kit.ContainerPlacementError
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.skija.*
import gui4s.desktop.skija.typeface.*
import io.github.humbleui.skija.*

object GridExample extends UIApp:
  val settings = WindowCreationSettings(
    title = "Gui4s nested containers example example",
    width = 620,
    height = 480,
  )

  def main(
            glfw: PostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
          ) : Resource[AppIO, DesktopWidget[AppIO, ApplicationRequest]] =
    for
      shaper <- createShaper[AppIO]
      cache : TextCache[AppIO] <- ScalacacheCache()
      typeface <- defaultTypeface[AppIO]
      spaceBetween: PlacementStrategy[AppIO, List] =
        PlacementStrategy.SpaceBetween(ContainerPlacementError.English)
      begin : OneElementPlacementStrategy[AppIO] =
        PlacementStrategy.Begin(0f)
      numbers = (1 to 10).toList
      textWidget = text(shaper, cache)
    yield
      column[AppIO][ApplicationRequest](
        verticalPlacementStrategy = spaceBetween,
        horizontalPlacementStrategy = begin,
        children =
          numbers.map:
            columnIndex =>
             row[AppIO][ApplicationRequest](
                horizontalPlacementStrategy = spaceBetween,
                verticalPlacementStrategy = begin,
                children =
                  numbers.map:
                    rowIndex =>
                      textWidget(
                        text = columnIndex.toString + ":" + rowIndex.toString,
                        style = SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))
                      )
              )
      )
  end main
end GridExample
