package me.katze.gui4s.example
package task

import me.katze.gui4s.widget
import me.katze.gui4s.widget.stateful.Path
import me.katze.gui4s.widget.RunnableIO

trait TaskSet[+F[_], -WidgetTask]:
  def pushTask(io: RunnableIO[WidgetTask]): F[Unit]
  
  def aliveTasksPaths: F[Set[Path]]
  
  def killTasksFor(path: Path): F[Unit]
end TaskSet

