package me.katze.gui4s.widget
package stateful

trait State[+Update[+_], +WidgetTask, RaiseableEvent, +ParentRaisableEvent, +Tree]:
  def render : Tree
  def handleEvent(event : RaiseableEvent) : Update[State[Update, WidgetTask, RaiseableEvent, ParentRaisableEvent, Tree]]
  def state : Any
  def mergeWithOldState(oldState: Any) : State[Update, WidgetTask, RaiseableEvent, ParentRaisableEvent, Tree]
end State