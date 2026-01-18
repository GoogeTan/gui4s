package gui4s.desktop.example

import cats.Id
import cats.data.EitherT
import cats.effect._
import cats.effect.std.Queue
import glfw4s.core._
import glfw4s.core.pure._
import glfw4s.jna.bindings.types._

import gui4s.core.kit.ContainerPlacementError

import gui4s.desktop.kit._
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.skija._
import gui4s.desktop.skija.typeface._

object GridExample extends UIApp:
  val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s nested containers example example",
    width = 620,
    height = 480,
  )

  def main(
            glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
          ) : Resource[AppIO, DesktopWidget[AppIO, Nothing]] =
    for
      shaper <- createShaper[AppIO]
      cache : TextCache[AppIO] <- ScalacacheCache()
      typeface <- defaultTypeface[AppIO]
      numbers = (1 to 10).toList
      text = TextWidget(shaper, cache)
      textStyle = SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))
    yield
      grid(numbers, numbers)(
        gridCell(text[Nothing](_, textStyle))
      )
  end main

  def grid[A, B, Event](as: List[A], bs: List[B])(f: (A, B) => DesktopWidget[AppIO, Event]) : DesktopWidget[AppIO, Event] =
    val spaceBetween: LinearContainerPlacementStrategy[AppIO, List] =
      LinearContainerPlacementStrategy.SpaceBetween[AppIO, List](ContainerPlacementError.English)
    val begin : OneElementLinearContainerPlacementStrategy[AppIO] =
      LinearContainerPlacementStrategy.Begin[AppIO, Id](0f)
    columnWidget[AppIO, Event](
      verticalPlacementStrategy = spaceBetween,
      horizontalPlacementStrategy = begin,
      children =
        as.map:
          columnElement =>
            rowWidget[AppIO, Event](
              horizontalPlacementStrategy = spaceBetween,
              verticalPlacementStrategy = begin,
              children =
                bs.map:
                  rowElement =>
                    f(columnElement, rowElement)
            )
    )
  end grid

  def gridCell[Event](textWidget: String => DesktopWidget[AppIO, Event])(row : Int, column : Int) : DesktopWidget[AppIO, Event] =
    textWidget(column.toString + ":" + row.toString)
  end gridCell
end GridExample
