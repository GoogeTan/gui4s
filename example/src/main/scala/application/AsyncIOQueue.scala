package me.katze.gui4s.example
package application

import cats.*
import cats.effect.*
import cats.effect.std.Queue
import cats.effect.syntax.all.{*, given}
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}

import scala.concurrent.ExecutionContext

final case class AsyncIOQueue[F[+_] : Monad : Async](queue: Queue[F, ? >: TaskFinished], context: ExecutionContext) extends IOQueue[F]:
  override def push[T](
                        returnPath: Path,
                        task      : WidgetTaskImpl[F, T]
                      ): F[Unit] =
    runAndPush(returnPath, task).startOn(context) *> Monad[F].unit // TODO Заменить start на что-то более подходящее
  end push
  
  private def runAndPush[T](returnPath: Path, task: WidgetTaskImpl[F, T]): F[Unit] =
    task match
      case WidgetTaskImpl.OneEvent(task) =>
        task.flatMap(offerTask(returnPath, _))
      case WidgetTaskImpl.ManyEvents(stream) =>
        stream
          .foreach(offerTask(returnPath, _))
          .compile
          .drain
    end match
  end runAndPush
  
  private def offerTask(returnPath: Path, value: Any) : F[Unit] =
    queue.offer(TaskFinished(returnPath, value))
  end offerTask  
end AsyncIOQueue
