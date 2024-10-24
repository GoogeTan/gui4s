package me.katze.gui4s.widget
package stateful

trait State[+Update[+_], RaiseableEvent, +Tree]:
  def render : Tree
  def handleEvent(event : RaiseableEvent) : Update[State[Update, RaiseableEvent, Tree]]
  def state : Any
  def mergeWithOldState(oldState: Any) : State[Update, RaiseableEvent, Tree]
end State