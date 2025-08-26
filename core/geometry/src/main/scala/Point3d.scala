package gui4s.core.geometry

import cats.kernel.*
import cats.syntax.all.*
import scala.math.Numeric.Implicits.*

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
final case class Point3d[+MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit):
  def this(point2d : Point2d[MeasurementUnit])(using N : Numeric[MeasurementUnit]) =
    this(point2d.x, point2d.y, N.zero)
  end this

  def this(point2d: Point2d[MeasurementUnit], z : MeasurementUnit) =
    this(point2d.x, point2d.y, z)
  end this

  def projectToXY[T >: MeasurementUnit] : Point2d[T] =
    Point2d(x, y)
  end projectToXY

  def +[NewMeasurementUnit >: MeasurementUnit](using S : Numeric[NewMeasurementUnit])(that : Point3d[NewMeasurementUnit]) : Point3d[NewMeasurementUnit] =
    Point3d(S.plus(x, that.x), S.plus(y, that.y), S.plus(z, that.z))
  end +

  def -[NewMeasurementUnit >: MeasurementUnit : Numeric](that : Point3d[NewMeasurementUnit]) : Point3d[NewMeasurementUnit] =
    this + (-that)
  end -

  def unary_-[NewMeasurementUnit >: MeasurementUnit](using N : Numeric[NewMeasurementUnit]): Point3d[NewMeasurementUnit] =
    Point3d(N.negate(x), N.negate(y), N.negate(z))
  end unary_-
end Point3d

object Point3d:
  given[MeasurementUnit : Group] : Group[Point3d[MeasurementUnit]] with
    override def combine(x: Point3d[MeasurementUnit], y: Point3d[MeasurementUnit]): Point3d[MeasurementUnit] =
      Point3d(x.x |+| y.x, x.y |+| y.y, x.z |+| y.z)
    end combine
    
    override def empty: Point3d[MeasurementUnit] =
      Point3d(Group[MeasurementUnit].empty, Group[MeasurementUnit].empty, Group[MeasurementUnit].empty)
    end empty
    
    override def inverse(x: Point3d[MeasurementUnit]): Point3d[MeasurementUnit] =
      Point3d(Group[MeasurementUnit].inverse(x.x), Group[MeasurementUnit].inverse(x.y), Group[MeasurementUnit].inverse(x.z))
    end inverse
  end given
end Point3d