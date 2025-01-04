package me.katze.gui4s.draw
package impure

import cats.Functor
import cats.syntax.all.*
import cats.data.*

type EitherT_[F[_], Error] = [T] =>> EitherT[F, Error, T]

final class EitherTImpure[F[_] : Functor, Error](initial : Impure[F]) extends Impure[EitherT_[F, Error]] with ImpureError[EitherT_[F, Error], Error]:
  override def impureTry[T](from: => Either[Error, T]): EitherT_[F, Error][T] = 
    EitherT(initial.impure(from))
  end impureTry
  
  override def impure[A](trunk: => A): EitherT_[F, Error][A] =
    EitherT(initial.impure(trunk).map(Right(_)))
  end impure
end EitherTImpure
