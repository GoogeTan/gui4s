package catnip

import cats.Functor
import cats.data.EitherT
import cats.syntax.functor.*

final class EitherTFFI[F[_] : Functor, Error](initial : ForeighFunctionInterface[F]) extends ForeighFunctionInterface[[T] =>> EitherT[F, Error, T]]:
  override def delay[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.delay(trunk).map(Right[Error, A]))
  end delay

  override def blocking[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.blocking(trunk).map(Right[Error, A]))
  end blocking

  override def interruptible[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.interruptible(trunk).map(Right[Error, A]))
  end interruptible

  override def interruptibleMany[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.interruptibleMany(trunk).map(Right[Error, A]))
  end interruptibleMany
end EitherTFFI
