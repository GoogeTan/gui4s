package me.katze.gui4s.example
package draw

import me.katze.gui4s.widget
import me.katze.gui4s.widget.stateful.{State, StatefulDraw}

given DelegateChildStatefulDraw[Draw]: StatefulDraw[Draw] with
  override def drawStateful[T](name : String, state : State[?, ?, T, ?], childTree: widget.Widget[?, Draw, ?, ?, ?]): Draw = childTree.draw
end DelegateChildStatefulDraw
