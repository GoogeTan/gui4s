package me.katze.gui4s.widget
package stateful

trait StatefulDraw[Draw]:
  def drawStateful[T](name : String, state : State[?, ?, Any, T, Any, Any], childTree : PlacedWidget[?, ?, Draw, Any, [A, B] =>> Any, Any, Nothing]) : Draw
end StatefulDraw
