package me.katze.gui4s.impure.cats

import cats.Functor
import cats.data.EitherT
import cats.syntax.functor.*
import me.katze.gui4s.impure.FFI

final class EitherTFFI[F[_] : Functor, Error](initial : FFI[F]) extends FFI[[T] =>> EitherT[F, Error, T]]:
  override def delay[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.delay(trunk).map(Right(_)))
  end delay

  override def blocking[A](trunk: => A): EitherT[F, Error, A] = 
    EitherT(initial.blocking(trunk).map(Right(_)))
  end blocking

  override def interruptible[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.interruptible(trunk).map(Right(_)))
  end interruptible

  override def interruptibleMany[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.interruptibleMany(trunk).map(Right(_)))
  end interruptibleMany
end EitherTFFI
