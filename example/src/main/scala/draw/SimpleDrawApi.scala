package me.katze.gui4s.example
package draw

import cats.kernel.Monoid
import cats.syntax.monoid.*

trait SimpleDrawApi[MU, F]:
  def text(x : MU, y : MU, text : String, style: TextStyle) : F
  
  def rectangle(x : MU, y : MU, width : MU, height : MU, color : Int) : F
  
  def beginDraw : F
  def endDraw : F
  
  def drawFrame(frame : F)(using m : Monoid[F]) : F =
    beginDraw |+| frame |+| endDraw
  end drawFrame
end SimpleDrawApi
