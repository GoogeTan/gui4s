package me.katze.gui4s.example
package task

import cats.*
import cats.data.*
import cats.effect.std.*
import cats.effect.kernel.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.RunnableIO
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.impl.WidgetTaskImpl.{ManyEvents, OneEvent}
import me.katze.gui4s.widget.stateful.Path


final case class TaskSetImpl[F[+_] : Monad, Task](
                                                    runningTasks : Ref[F, PathMap[IOOnThread[F]]],
                                                    startTask : (Path, Task) => F[Fiber[F, Throwable, Unit]]
                                                  ) extends TaskSet[F, Task]:
  override def aliveTasksPaths: F[Set[Path]] =
    runningTasks.get.map(_.keys)
  end aliveTasksPaths

  override def killTask(path: Path): F[Unit] =
    runningTasks.modify(_.remove(path)).flatMap(_.traverse_(_.fiberControl.cancel))
  end killTask

  override def pushTask(io: RunnableIO[Task]): F[Unit] =
    for
      fiber <- startTask(io.path, io.io)
      _ <- runningTasks.update(_.add(io.path, IOOnThread(io.keepAliveAfterWidgetDetach, fiber)))
    yield ()
  end pushTask
end TaskSetImpl
