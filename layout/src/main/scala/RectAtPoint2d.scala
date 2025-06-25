package me.katze.gui4s.layout

import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

final case class RectAtPoint2d[MeasurementUnit](rect: Rect[MeasurementUnit], point: Point2d[MeasurementUnit]):
  def startX : MeasurementUnit = point.y
  def startY : MeasurementUnit = point.y
  def endX(using N : Numeric[MeasurementUnit]) : MeasurementUnit = rect.width + point.x
  def endY(using N : Numeric[MeasurementUnit]) : MeasurementUnit = rect.height + point.y


  def isIn(point2d: Point2d[MeasurementUnit])(using N : Numeric[MeasurementUnit]) =
    startX <= point2d.x && point2d.x <= endX
      && startY <= point2d.y && point2d.y <= endY
  end isIn
end RectAtPoint2d

