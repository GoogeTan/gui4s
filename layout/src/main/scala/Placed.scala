package me.katze.gui4s.layout

import cats.Functor
import me.katze
final case class Placed[+MeasurementUnit, +T](value : T, coordinate : Point3d[MeasurementUnit], size : Rect[MeasurementUnit]):
  def this(sized : Sized[MeasurementUnit, T], coordinate : Point3d[MeasurementUnit]) = this(sized.value, coordinate, Rect(sized.width, sized.height))
  def this(sized : Sized[MeasurementUnit, T], x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit) = this(sized, Point3d(x, y, z))

  def x : MeasurementUnit = coordinate.x
  def y : MeasurementUnit = coordinate.y
  def z : MeasurementUnit = coordinate.z

  def width : MeasurementUnit = size.width
  def height : MeasurementUnit = size.height
  
  def mapValue[A](f : T => A) : Placed[MeasurementUnit, A] =
    copy(value = f(value))
end Placed

given[M] : Functor[Placed[M, *]] with
  override def map[A, B](fa: Placed[M, A])(f: A => B): Placed[M, B] =
    fa.mapValue(f)
  end map
end given