package me.katze.gui4s.layout

import cats.{Group, Semigroup}
import cats.kernel.Monoid
import cats.syntax.all.*

final case class Point3d[+MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit):
  def projectToXY[T >: MeasurementUnit] : Point2d[T] =
    Point2d(x, y)
  end projectToXY

  def +[NewMeasurementUnit >: MeasurementUnit : Numeric as S](that : Point3d[NewMeasurementUnit]) : Point3d[NewMeasurementUnit] =
    Point3d(S.plus(x, that.x), S.plus(y, that.y), S.plus(z, that.z))
  end +

  def -[NewMeasurementUnit >: MeasurementUnit : Numeric](that : Point3d[NewMeasurementUnit]) : Point3d[NewMeasurementUnit] =
    this + (-that)
  end -

  def unary_-[NewMeasurementUnit >: MeasurementUnit : Numeric as N] : Point3d[NewMeasurementUnit] =
    Point3d(N.negate(x), N.negate(y), N.negate(z))
  end unary_-
end Point3d
