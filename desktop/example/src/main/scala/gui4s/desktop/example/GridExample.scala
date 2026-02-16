package gui4s.desktop.example

import cats.Id
import cats.effect.*
import cats.effect.std.Queue
import glfw4s.core.*
import glfw4s.core.pure.*
import glfw4s.jna.bindings.types.*
import gui4s.core.geometry.Point2d
import gui4s.core.kit.ContainerPlacementError
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.*
import gui4s.desktop.skija.typeface.*

object GridExample extends UIApp:
  val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
    title = "Gui4s nested containers example example",
    width = 620,
    height = 480,
  )

  def main(
            glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
          ) : Resource[IO, DesktopWidget[Nothing]] =
    for
      shaper <- createShaper[IO]
      cache : TextCache[IO] <- ScalacacheCache()
      typeface <- defaultTypeface[IO]
      numbers = (1 to 10).toList
      text = TextWidget(shaper, cache)
      textStyle = SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))
    yield
      grid((1 to 10).toList, ('a' to 'f').toList)(
        gridCell(text[Nothing](_, textStyle))
      )
  end main

  def grid[A, B, Event](as: List[A], bs: List[B])(f: (A, B) => DesktopWidget[Event]) : DesktopWidget[Event] =
    val spaceBetween: LinearContainerPlacementStrategy[List] =
      LinearContainerPlacementStrategy.SpaceBetween[List](ContainerPlacementError.English)
    val begin : OneElementLinearContainerPlacementStrategy =
      LinearContainerPlacementStrategy.Begin[Id](0f)
    columnWidget[Event](
      verticalPlacementStrategy = spaceBetween,
      horizontalPlacementStrategy = begin,
      children =
        as.map:
          columnElement =>
            rowWidget[Event](
              horizontalPlacementStrategy = spaceBetween,
              verticalPlacementStrategy = begin,
              children =
                bs.map:
                  rowElement =>
                    f(columnElement, rowElement)
                      .border(
                        Shapes.roundedCorners(50f),
                        Brush.solid(0xFF454649), 
                        StrokeOptions(width = 4f)
                      )
            )
    ).paintOnBackgroundWith(Brush.linearGradient(
        Point2d[Float](0f, 0f),
        Point2d[Float](500f, 500f),
        List(0xFF242424, 0xFF343434, 0xFF445444),
      )
    )
  end grid

  def gridCell[Event](textWidget: String => DesktopWidget[Event])(row : Int, column : Char) : DesktopWidget[Event] =
    textWidget(column.toString + ":" + row.toString)
  end gridCell
end GridExample
