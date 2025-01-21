package me.katze.gui4s.example
package task

import update.CatsFiber

import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.widget.stateful.Path

// TODO Разделить на 2 типа. При создании в виджете не нужна возможность запуска и, следовательно, impure.
final case class RunnableIO[+F[+_] : Concurrent, +T](
                                                        io: F[T],
                                                        override val owner: Path,
                                                        override val keepAliveAfterOwnerDetach: Boolean,
                                                        impure: Impure[F]
                                                      ) extends Task[F, CatsFiber[F, Throwable, T]]:
  override def start: F[CatsFiber[F, Throwable, T]] =
    Concurrent[F].start(io).map(CatsFiber(_, impure))
  end start
end RunnableIO