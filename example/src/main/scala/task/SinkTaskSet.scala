package me.katze.gui4s.example
package task

import update.{MultiMap, StandardMapWrapperMultiMap}

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.std.QueueSink
import cats.syntax.all.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.stateful.TaskFinished

def runInQueueTaskSet[F[+_] : Concurrent, DownEvent >: TaskFinished](
                                                                      taskMap : Ref[F, MultiMap[Path, IOOnThread[Fiber[F]]]],
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

def runInQueueTaskSet[F[+_] : Concurrent, DownEvent >: TaskFinished](
                                                                      queue  : QueueSink[F, DownEvent],
                                                                      impure : Impure[F]
                                                                    ): F[TaskSet[F, RunnableIO[EventProducingEffectT[F], Any]]] =
  Ref[F]
    .of(new StandardMapWrapperMultiMap())
    .map(runInQueueTaskSet(_, queue, impure))
end runInQueueTaskSet


def offerTask[F[+_]](queue: QueueSink[F, ? >: TaskFinished], at: Path, taskResult: Any): F[Unit] =
  queue.offer(TaskFinished(at, taskResult))
end offerTask