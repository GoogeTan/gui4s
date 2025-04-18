package me.katze.gui4s.widget
package stateful

trait State[+Update[+_], Dealloc, RaiseableEvent, +Tree]:
  def render : Tree
  def handleEvent(event : RaiseableEvent) : Update[State[Update, Dealloc, RaiseableEvent, Tree]]
  def state : Any
  def dealloc : Dealloc
  def mergeWithOldState(oldState: Any) : State[Update, Dealloc, RaiseableEvent, Tree]
end State