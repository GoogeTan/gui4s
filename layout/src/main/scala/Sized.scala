package me.katze.gui4s.layout

import cats.Comonad
import me.katze.gui4s.geometry.Rect

final case class Sized[+MeasurementUnit, +T](value : T, size : Rect[MeasurementUnit]):
  def this(value : T, x : MeasurementUnit, y : MeasurementUnit) =
    this(value, Rect(x, y))
  end this
  
  def width : MeasurementUnit = size.width
  def height : MeasurementUnit = size.height
  
  def mapValue[B](f : T => B) : Sized[MeasurementUnit, B] =
    copy(value = f(value))
  end mapValue

  /**
   * Возвращает ширину вдоль оси
   * @param axis
   * @return
   */
  def lengthAlong(axis : Axis) : MeasurementUnit =
    axis match
      case Axis.Vertical => height
      case Axis.Horizontal => width
    end match
  end lengthAlong
  
  def lengthAlongAnother(axis: Axis): MeasurementUnit =
    lengthAlong(axis.another)
  end lengthAlongAnother
end Sized

given[MeasurementUnit]: Comonad[Sized[MeasurementUnit, *]] with
  override def coflatMap[A, B](fa: Sized[MeasurementUnit, A])(f: Sized[MeasurementUnit, A] => B): Sized[MeasurementUnit, B] =
    fa.mapValue(_ => f(fa))
  end coflatMap

  override def extract[A](x: Sized[MeasurementUnit, A]): A =
    x.value
  end extract

  override def map[A, B](fa: Sized[MeasurementUnit, A])(f: A => B): Sized[MeasurementUnit, B] =
    fa.mapValue(f)
  end map
end given