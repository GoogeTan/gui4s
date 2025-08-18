package me.katze.gui4s.layout

import cats.Functor
import me.katze
import me.katze.gui4s.geometry.{Point2d, Point3d, Rect}

final case class Placed[+MeasurementUnit, +T](value : T, coordinate : Point3d[MeasurementUnit], size : Rect[MeasurementUnit]):
  def this(sized : Sized[MeasurementUnit, T], coordinate : Point3d[MeasurementUnit]) = 
    this(sized.value, coordinate, Rect(sized.width, sized.height))
  def this(sized : Sized[MeasurementUnit, T], coordinate : Point2d[MeasurementUnit], z : MeasurementUnit) = 
    this(sized.value, new Point3d(coordinate, z), Rect(sized.width, sized.height))
  def this(sized : Sized[MeasurementUnit, T], x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit) = 
    this(sized, Point3d(x, y, z))

  def x : MeasurementUnit = coordinate.x
  def y : MeasurementUnit = coordinate.y
  def z : MeasurementUnit = coordinate.z

  def width : MeasurementUnit = size.width
  def height : MeasurementUnit = size.height

  def endX[MU >: MeasurementUnit](using n: Numeric[MU]) : MU =
    n.plus(x, width)
  end endX

  def endY[MU >: MeasurementUnit](using n : Numeric[MU]) : MU =
    n.plus(y, height)
  end endY

  def mapValue[A](f : T => A) : Placed[MeasurementUnit, A] =
    copy(value = f(value))
end Placed

given[M] : Functor[Placed[M, *]] with
  override def map[A, B](fa: Placed[M, A])(f: A => B): Placed[M, B] =
    fa.mapValue(f)
  end map
end given