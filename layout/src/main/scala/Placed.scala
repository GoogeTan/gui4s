package me.katze.gui4s.layout

import cats.Functor
import me.katze
final case class Placed[+MeasurementUnit, +T](value : T, x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit, width : MeasurementUnit, height : MeasurementUnit):
  def this(sized : Sized[MeasurementUnit, T], x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit) = this(sized.value, x, y, z, sized.width, sized.height)

  def rect : Rect[MeasurementUnit] = Rect(x, y, width, height)

  def mapValue[A](f : T => A) : Placed[MeasurementUnit, A] =
    copy(value = f(value))
end Placed

given[M] : Functor[Placed[M, *]] with
  override def map[A, B](fa: Placed[M, A])(f: A => B): Placed[M, B] =
    fa.mapValue(f)
  end map
end given