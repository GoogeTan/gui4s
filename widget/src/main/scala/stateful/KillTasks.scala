package me.katze.gui4s.widget
package stateful

trait KillTasks[T]:
  def killDetachableTasks(path : Path) : T
end KillTasks

