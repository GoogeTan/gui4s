package me.katze.gui4s.example
package draw

final case class TextStyle(size : Int, color : Int, weight : Int)

/**
 * Тестовое апи для рисования. Координаты считаются от верхнего левого угла.
 */
trait SimpleDrawApi[F[_]]:
  def text(x : Int, y : Int, text : String, style: TextStyle) : F[Unit]
  
  def rectangle(x : Int, y : Int, width : Int, height : Int, color : Int) : F[Unit]
  
  def beginDraw : F[Unit]
  def endDraw : F[Unit]
end SimpleDrawApi
