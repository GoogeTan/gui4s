package catnip.cats.effect

import catnip.ForeighFunctionInterface
import cats.effect.Async
import cats.effect.syntax.async.*

import scala.concurrent.ExecutionContext

final class ContextForeighFunctionInterface[F[_] : Async](ec : ExecutionContext, initial : ForeighFunctionInterface[F]) extends ForeighFunctionInterface[F]:
  override def delay[A](trunk: => A): F[A] =
    initial.delay(trunk).evalOn(ec)
  end delay

  override def blocking[A](trunk: => A): F[A] =
    initial.blocking(trunk).evalOn(ec)
  end blocking

  override def interruptible[A](trunk: => A): F[A] =
    initial.interruptible(trunk).evalOn(ec)
  end interruptible

  override def interruptibleMany[A](trunk: => A): F[A] =
    initial.interruptibleMany(trunk).evalOn(ec)
  end interruptibleMany
end ContextForeighFunctionInterface
