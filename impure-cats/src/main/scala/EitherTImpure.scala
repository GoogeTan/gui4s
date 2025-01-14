package me.katze.gui4s.impure.cats

import cats.Functor
import cats.data.EitherT
import cats.syntax.functor.*
import me.katze.gui4s.impure.{Impure, ImpureError}

final class EitherTImpure[F[_] : Functor, Error](initial : Impure[F]) extends Impure[[T] =>> EitherT[F, Error, T]] with ImpureError[[T] =>> EitherT[F, Error, T], Error]:
  override def impureTry[T](from: => Either[Error, T]): EitherT[F, Error, T] = 
    EitherT(initial.impure(from))
  end impureTry
  
  override def impure[A](trunk: => A): EitherT[F, Error, A] =
    EitherT(initial.impure(trunk).map(Right(_)))
  end impure
end EitherTImpure
