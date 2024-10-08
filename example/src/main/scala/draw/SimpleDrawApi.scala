package me.katze.gui4s.example
package draw

trait SimpleDrawApi[MU, F]:
  def text(x : MU, y : MU, text : String, style: TextStyle) : F
  
  def rectangle(x : MU, y : MU, width : MU, height : MU, color : Int) : F
  
  def beginDraw : F
  def endDraw : F
end SimpleDrawApi
