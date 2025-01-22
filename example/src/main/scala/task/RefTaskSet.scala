package me.katze.gui4s.example
package task

import update.MultiMap

import cats.*
import cats.effect.Ref
import cats.syntax.all.*
import me.katze.gui4s.widget.stateful.Path

final case class RefTaskSet[F[+_] : Monad, -T <: Task[F, FiberControl], FiberControl <: Fiber[F]](
                                                                                                    runningTaskSet: Ref[F, MultiMap[Path, IOOnThread[FiberControl]]]
                                                                                                  ) extends TaskSet[F, T]:
  override def killTasksFor(path: Path): F[Unit] =
    runningTaskSet
      .modify(_.removeAll(path))
      .flatMap(deletedRunningTasks =>
        deletedRunningTasks
          .filterNot(_.keepAfterWidgetDeath).toList
          .traverse_(_.fiberControl.cancel)
      )
  end killTasksFor

  override def pushTask(io: T): F[Unit] =
    for
      fiber <- io.start
      _ <- runningTaskSet.update(_.add(io.owner, IOOnThread(io.keepAliveAfterOwnerDetach, fiber)))
    yield ()
  end pushTask
end RefTaskSet
