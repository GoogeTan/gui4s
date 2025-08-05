package me.katze.gui4s.geometry

import cats.*
import cats.data.*
import cats.syntax.all.*
import scala.math.Numeric.Implicits.*

final case class Rect[+MeasurementUnit](width : MeasurementUnit, height : MeasurementUnit):
  def this(mainAxis : Axis, mainAxisLength : MeasurementUnit, additionalAxisLength : MeasurementUnit) =
    this(
      if mainAxis === Axis.Horizontal then mainAxisLength else additionalAxisLength, 
      if mainAxis === Axis.Horizontal then additionalAxisLength else mainAxisLength
    )

extension[MeasurementUnit : Numeric](value : Rect[MeasurementUnit])
  def +(another : Rect[MeasurementUnit]) : Rect[MeasurementUnit] =
    Rect(value.width + another.width, value.height + another.height)
  end +

  def unary_- : Rect[MeasurementUnit] =
    Rect(-value.width, -value.height)
  end unary_-

  def -(another : Rect[MeasurementUnit]) : Rect[MeasurementUnit] =
    (-value) + another
  end -
end extension
