package gui4s.core.geometry

import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

final case class RectAtPoint2d[MeasurementUnit](rect: Rect[MeasurementUnit], point: Point2d[MeasurementUnit]):
  def startX : MeasurementUnit = point.y
  def startY : MeasurementUnit = point.y
  def endX(using N : Numeric[MeasurementUnit]) : MeasurementUnit = rect.width + point.x
  def endY(using N : Numeric[MeasurementUnit]) : MeasurementUnit = rect.height + point.y


  def containsPoint(point2d: Point2d[MeasurementUnit])(using N : Numeric[MeasurementUnit]): Boolean =
    startX <= point2d.x && point2d.x <= endX && startY <= point2d.y && point2d.y <= endY
  end containsPoint
end RectAtPoint2d

