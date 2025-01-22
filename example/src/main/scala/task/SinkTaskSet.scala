package me.katze.gui4s.example
package task

import update.MultiMap

import cats.effect.kernel.Concurrent
import cats.effect.std.{AtomicCell, QueueSink}
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}

def runInQueueTaskSet[F[+_] : Concurrent, DownEvent >: TaskFinished](
                                                                      taskMap : AtomicCell[F, MultiMap[Path, IOOnThread[Fiber[F]]]],
                                                                      queue : QueueSink[F, DownEvent],
                                                                      impure : Impure[F]
                                                                    ) : TaskSet[F, RunnableIO[EventProducingEffectT[F], Any]] =
  ContramapTaskSet(
    RefTaskSet[F, RunnableIO[F, Any], Fiber[F]](
      taskMap
    ),
    a => RunnableIO(a.io(offerTask(queue, a.owner, _ : Any)), a.owner, a.keepAliveAfterOwnerDetach, impure)
  )
end runInQueueTaskSet


def offerTask[F[+_]](queue: QueueSink[F, ? >: TaskFinished], at: Path, taskResult: Any): F[Unit] =
  queue.offer(TaskFinished(at, taskResult))
end offerTask