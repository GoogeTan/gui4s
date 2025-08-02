package me.katze.gui4s.geometry

import cats.*
import cats.data.*
import cats.syntax.all.*

final case class Rect[+MeasurementUnit](width : MeasurementUnit, height : MeasurementUnit):
  def this(mainAxis : Axis, mainAxisLength : MeasurementUnit, additionalAxisLength : MeasurementUnit) =
    this(
      if mainAxis === Axis.Horizontal then mainAxisLength else additionalAxisLength, 
      if mainAxis === Axis.Vertical then additionalAxisLength else mainAxisLength
    )
    
