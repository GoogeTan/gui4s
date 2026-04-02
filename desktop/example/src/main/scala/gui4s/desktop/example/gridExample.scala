package gui4s.desktop.example

import cats.Id

import gui4s.core.geometry.Point2d
import gui4s.core.kit.ContainerPlacementError

import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.kit.widgets.decorator._
import gui4s.desktop.skija._

def gridExample(
                text : TextWidget,
                typeface : Typeface
) : DesktopWidget[Nothing] =
  val textStyle = SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))
  val numbers = (1 to 10).toList
  grid((1 to 10).toList, ('a' to 'f').toList)(
    gridCell(text[Nothing](_, textStyle))
  )
end gridExample

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
                      Shape.roundedCorners(50f),
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
