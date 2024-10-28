package me.katze.gui4s.widget
package stateful

trait StatefulDraw[Draw]:
  def drawStateful[T](name : String, state : State[?, T, ?], childTree : Widget[?, Draw, ?, ?, ?, ?]) : Draw
end StatefulDraw
