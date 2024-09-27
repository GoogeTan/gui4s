package me.katze.gui4s.example

import cats.*
import cats.data.*
import cats.effect.std.*
import cats.effect.kernel.*
import cats.syntax.all.{*, given}
import me.katze.gui4s.widget
import me.katze.gui4s.widget.{RunnableIO, impl}
import me.katze.gui4s.widget.impl.WidgetTaskImpl
import me.katze.gui4s.widget.impl.WidgetTaskImpl.{ManyEvents, OneEvent}
import me.katze.gui4s.widget.stateful.{Path, TaskFinished}

import scala.concurrent.ExecutionContext

trait IOMaster[F[_], WidgetTask]:
  def pushIO(io : RunnableIO[WidgetTask]): F[Unit]
  
  def alive: F[Set[Path]]
  
  def detach(path: Path): F[Unit]
end IOMaster

case class IOOnThread[F[_]](keepAfterWidgetDeath : Boolean, join : Fiber[F, Throwable, Unit])

final case class IOMasterImpl[F[+_] : Concurrent](
                                                  runningTasks : Ref[F, Map[Path, IOOnThread[F]]],
                                                  // excecutionContext : ExecutionContext,TODO
                                                  pushFinishedEvent : (Path, Any) => F[Unit]
                                                ) extends IOMaster[F, WidgetTaskImpl[F, Any]]:
  override def alive: F[Set[Path]] =
    runningTasks.get.map(_.keys.toSet)
  end alive 
  
  override def detach(path: Path): F[Unit] =
    runningTasks.modify(current => (current.-(path), current(path).join)).flatMap(_.cancel)
  end detach
  
  override def pushIO(io: RunnableIO[WidgetTaskImpl[F, Any]]): F[Unit] =
    io.io match
      case OneEvent(value) => 
        for
          fiber <- Concurrent[F].start(value.flatMap(pushFinishedEvent(io.path, _)))
          _ <- runningTasks.update(_.+((io.path, IOOnThread(io.keepAliveAfterWidgetDetach, fiber))))
        yield ()  
      case ManyEvents(stream) =>
        for
          fiber <- Concurrent[F].start(stream.compile.toVector.flatMap(pushFinishedEvent(io.path, _)))
          _ <- runningTasks.update(_.+((io.path, IOOnThread(io.keepAliveAfterWidgetDetach, fiber))))
        yield ()  
    end match
  end pushIO
end IOMasterImpl
