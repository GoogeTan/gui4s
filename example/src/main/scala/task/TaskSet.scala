package me.katze.gui4s.example
package task

import cats.effect.Fiber
import me.katze.gui4s.widget
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.impl.WidgetTaskImpl.{ManyEvents, OneEvent}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}
import me.katze.gui4s.widget.{RunnableIO, impl}

import scala.concurrent.ExecutionContext

trait TaskSet[F[_], WidgetTask]:
  def pushTask(io: RunnableIO[WidgetTask]): F[Unit]
  
  def aliveTasksPaths: F[Set[Path]]
  
  def killTask(path: Path): F[Unit]
end TaskSet

