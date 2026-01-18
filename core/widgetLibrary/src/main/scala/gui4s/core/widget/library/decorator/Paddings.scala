package gui4s.core.widget.library.decorator

import scala.math.Numeric.Implicits._

import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect

final case class Paddings[Padding](left : Padding, top : Padding, right : Padding, bottom : Padding):
  def verticalLength(using Numeric[Padding]) : Padding =
    top + bottom
  end verticalLength

  def horizontalLength(using Numeric[Padding]) : Padding =
    left + right
  end horizontalLength

  def topLeftCornerShift : Point2d[Padding] =
    Point2d(left, top)
  end topLeftCornerShift

  def addedBoundsRect(using Numeric[Padding]) : Rect[Padding] =
    Rect(horizontalLength, verticalLength)
  end addedBoundsRect

  def map[NewPadding](f : Padding => NewPadding) : Paddings[NewPadding] =
    Paddings(f(left), f(top), f(right), f(bottom))
  end map
end Paddings
