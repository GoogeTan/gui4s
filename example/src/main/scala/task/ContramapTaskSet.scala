package me.katze.gui4s.example
package task
import me.katze.gui4s.widget.stateful.Path

final class ContramapTaskSet[+F[+_], -A, -B](initial : TaskSet[F, A], f : B => A) extends TaskSet[F, B]:
  override def killTasksFor(path: Path): F[Unit] =
    initial.killTasksFor(path)
  end killTasksFor

  override def pushTask(io: B): F[Unit] =
    initial.pushTask(f(io))
  end pushTask
end ContramapTaskSet
