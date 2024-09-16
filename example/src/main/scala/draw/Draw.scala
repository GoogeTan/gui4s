package me.katze.gui4s.example
package draw

trait Draw[F[+_], T]:
  def clip[K](x : T, y : T, width : T, height : T, body : F[K]) : F[K]
  def viewport[K](x : T, y : T, width : T, height : T, body : F[K]) : F[K]
  def clipViewport[K](x : T, y : T, width : T, height : T, body : F[K]) : F[K]
  def drawText(text : String, x : T, y : T) : F[Unit]
end Draw
