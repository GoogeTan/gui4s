package me.katze.gui4s.geometry

import scala.reflect.Typeable

final case class Point2d[+MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit):
  def +[T >: MeasurementUnit](other: Point2d[T])(using numeric: Numeric[T]): Point2d[T] =
    Point2d(numeric.plus(x, other.x), numeric.plus(y, other.y))

  def -[T >: MeasurementUnit](other: Point2d[T])(using numeric: Numeric[T]): Point2d[T] =
    Point2d(numeric.minus(x, other.x), numeric.minus(y, other.y))

  def unary_-[T >: MeasurementUnit](using numeric: Numeric[T]): Point2d[T] =
    Point2d(numeric.negate(x), numeric.negate(y))


@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given[MeasurementUnit : Typeable] : Typeable[Point2d[MeasurementUnit]] = (value : Any) => value match
  case Point2d[Any](x : MeasurementUnit, y : MeasurementUnit) => Some(Point2d(x, y).asInstanceOf[value.type & Point2d[MeasurementUnit]])
  case _ => None
