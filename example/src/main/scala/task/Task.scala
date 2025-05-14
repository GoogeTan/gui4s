package me.katze.gui4s.example
package task

import me.katze.gui4s.widget.Path

trait Task[+F[+_], +Fiber]:
  def owner: Path

  def keepAliveAfterOwnerDetach: Boolean

  def start: F[Fiber]
end Task

