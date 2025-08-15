package me.katze.gui4s.widget.library

import me.katze.gui4s.geometry.{Point2d, Rect}

import scala.math.Numeric.Implicits.*

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
