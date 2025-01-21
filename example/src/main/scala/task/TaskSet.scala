package me.katze.gui4s.example
package task

import me.katze.gui4s.widget.stateful.Path

trait TaskSet[+F[_], -WidgetTask]:
  def pushTask(io: WidgetTask): F[Unit]
  
  def killTasksFor(path: Path): F[Unit]
end TaskSet

