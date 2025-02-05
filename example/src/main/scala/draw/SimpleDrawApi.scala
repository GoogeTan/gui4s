package me.katze.gui4s.example
package draw

import cats.kernel.Monoid
import cats.syntax.monoid.*

trait SimpleDrawApi[MeasurementUnit, F]:
  def text(x : MeasurementUnit, y : MeasurementUnit, text : String, style: TextStyle) : F
  
  def rectangle(x : MeasurementUnit, y : MeasurementUnit, width : MeasurementUnit, height : MeasurementUnit, color : Int) : F
  
  def beginDraw : F
  def endDraw : F
  
  def drawFrame(frame : F)(using m : Monoid[F]) : F =
    beginDraw |+| frame |+| endDraw
  end drawFrame
end SimpleDrawApi
