package me.katze.gui4s.geometry

import cats.*
import cats.data.*
import cats.syntax.all.*
import me.katze.gui4s
import me.katze.gui4s.geometry

import scala.math.Numeric.Implicits.*

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
final case class Rect[+MeasurementUnit](width : MeasurementUnit, height : MeasurementUnit):
  def this(mainAxis : Axis, mainAxisLength : MeasurementUnit, additionalAxisLength : MeasurementUnit) =
    this(
      if mainAxis === Axis.Horizontal then mainAxisLength else additionalAxisLength, 
      if mainAxis === Axis.Horizontal then additionalAxisLength else mainAxisLength
    )

  def this(point : Point2d[MeasurementUnit]) =
    this(point.x, point.y)
  end this

  def along(axis : Axis) : MeasurementUnit =
    axis match
      case gui4s.geometry.Axis.Vertical => height
      case gui4s.geometry.Axis.Horizontal => width
    end match
  end along

  def mapAlong[T >: MeasurementUnit](axis: Axis, f : T  => T) : Rect[T] =
    axis match
      case Axis.Vertical =>
        copy(
          height = f(height)
        )
      case Axis.Horizontal =>
        copy(
          width = f(width)
        )
    end match
  end mapAlong

  def cut[T >: MeasurementUnit](width : T, height : T)(using N : Numeric[T]) : Rect[T] =
    Rect(N.minus(this.width, width), N.minus(this.height, height))
  end cut

  def asPoint2d : Point2d[MeasurementUnit] =
    Point2d(width, height)
  end asPoint2d
end Rect

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

object Rect:
  given rectEq[MeasurementUnit : Eq] : Eq[Rect[MeasurementUnit]] = Eq.by(r => (r.width, r.height))
end Rect
