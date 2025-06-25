package me.katze.gui4s.layout

final case class Point3d[+MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit, z : MeasurementUnit):
  def projectToXY[T >: MeasurementUnit] : Point2d[T] =
    Point2d(x, y)
  end projectToXY
end Point3d

