package me.katze.gui4s.layout
package bound

import me.katze
import me.katze.gui4s
import me.katze.gui4s.geometry.{Axis, InfinityOr, Point2d, Rect}
import me.katze.gui4s.{geometry, layout}

final case class Bounds[+MeasurementUnit](horizontal : InfinityOr[MeasurementUnit], vertical : InfinityOr[MeasurementUnit]):
  def this(width : MeasurementUnit, height : MeasurementUnit)(using Numeric[MeasurementUnit]) =
    this(new InfinityOr(width), new InfinityOr(height))
  end this

  def this(rect : Rect[MeasurementUnit])(using Numeric[MeasurementUnit]) =
    this(rect.width, rect.height)
  end this

  def this(width : Option[MeasurementUnit], height : Option[MeasurementUnit])(using Numeric[MeasurementUnit]) =
    this(InfinityOr(width), InfinityOr(height))
  end this

  def asPoint2d : Point2d[InfinityOr[MeasurementUnit]] =
    Point2d(horizontal, vertical)
  end asPoint2d
  
  def cutAlong[T >: MeasurementUnit](axis: Axis, amount : T)(using Numeric[T]) : Bounds[T] =
    axis match
      case Axis.Vertical => 
        copy(
          vertical = vertical.minus(amount)
        )
      case Axis.Horizontal =>
        copy(
          horizontal = horizontal.minus(amount)
        )
    end match
  end cutAlong
  
  def cut[T >: MeasurementUnit](horizontal : T, vertical : T)(using Numeric[T]) : Bounds[T] =
    Bounds(
      this.horizontal.minus(horizontal),
      this.vertical.minus(vertical)
    )
  
  def along(axis : Axis) : InfinityOr[MeasurementUnit] =
    axis match
      case Axis.Vertical => horizontal
      case geometry.Axis.Horizontal => vertical
    end match
  end along
end Bounds
