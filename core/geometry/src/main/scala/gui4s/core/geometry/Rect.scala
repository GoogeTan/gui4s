package gui4s.core.geometry

import scala.math.Numeric.Implicits._
import scala.math.Ordered.orderingToOrdered

import cats._
import cats.syntax.all._

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
      case Axis.Vertical => height
      case Axis.Horizontal => width
    end match
  end along
  
  def map[K](f : MeasurementUnit => K) : Rect[K] =
    Rect(f(width), f(height))
  end map

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

  def cut[T, K](width : T, height : T, cut : (MeasurementUnit, T) => K) : Rect[K] =
    Rect(cut(this.width, width), cut(this.height, height))
  end cut

  def toPoint2d : Point2d[MeasurementUnit] =
    Point2d(width, height)
  end toPoint2d
  
  def contains[T >: MeasurementUnit](point : Point2d[T])(using N : Numeric[T]) : Boolean =
      point.x >= N.zero && point.x <= width && point.y >= N.zero && point.y <= height
  end contains
  
  def +[T >: MeasurementUnit](another : Rect[T])(using N : Numeric[T]) : Rect[T] =
      Rect(N.plus(width, another.width), N.plus(height, another.height))
  end +
  
  def +[T >: MeasurementUnit](point : Point2d[T])(using N : Numeric[T]) : Rect[T] =
    this + point.toRect
  end +  
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
