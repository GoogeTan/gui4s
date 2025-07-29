package me.katze.gui4s.geometry

import scala.reflect.Typeable

final case class Point2d[+MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit)

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given[MeasurementUnit : Typeable] : Typeable[Point2d[MeasurementUnit]] = (value : Any) => value match
  case Point2d[MeasurementUnit](x : MeasurementUnit, y : MeasurementUnit) => Some(Point2d(x, y).asInstanceOf[value.type & Point2d[MeasurementUnit]])
  case _ => None