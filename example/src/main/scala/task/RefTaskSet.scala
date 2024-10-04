package me.katze.gui4s.example
package task

import cats.*
import cats.data.*
import cats.effect.std.*
import cats.effect.kernel.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.RunnableIO
import me.katze.gui4s.widget.stateful.Path


final case class RefTaskSet[F[+_] : Monad, Task](
                                                  runningTaskSet: Ref[F, MultiMap[Path, IOOnThread[F]]],
                                                  startTask     : (Path, Task) => F[Fiber[F, Throwable, Unit]]
                                                ) extends TaskSet[F, Task]:
  override def aliveTasksPaths: F[Set[Path]] =
    runningTaskSet.get.map(_.keys)
  end aliveTasksPaths

  override def killTask(path: Path): F[Unit] =
    runningTaskSet
      .modify(_.remove(path))
      .flatMap(deletedRunningTasks =>
        deletedRunningTasks
          .filterNot(_.keepAfterWidgetDeath).toList
          .traverse_(_.fiberControl.cancel)
      )
  end killTask

  override def pushTask(io: RunnableIO[Task]): F[Unit] =
    for
      fiber <- startTask(io.path, io.io)
      _ <- runningTaskSet.update(_.add(io.path, IOOnThread(io.keepAliveAfterWidgetDetach, fiber)))
    yield ()
  end pushTask
end RefTaskSet
