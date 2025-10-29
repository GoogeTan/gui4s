package catnip
package syntax

import catnip.syntax.monad.MonadErrorC
import cats.data.{EitherT, StateT}
import cats.syntax.all.*
import cats.{FlatMap, Functor, ~>}

object functionk:
  def runEitherTK[F[_] : MonadErrorC[Error] as ME, Error] : EitherT[F, Error, *] ~> F =
    new ~>[EitherT[F, Error, *], F]:
      override def apply[A](fa: EitherT[F, Error, A]): F[A] =
        fa.value.flatMap(ME.fromEither)
      end apply
    end new
  end runEitherTK

  def mapErrorK[F[_] : Functor, Error1, Error2](f : Error1 => Error2) : EitherT[F, Error1, *] ~> EitherT[F, Error2, *] =
    new ~>[EitherT[F, Error1, *], EitherT[F, Error2, *]]:
      override def apply[A](fa: EitherT[F, Error1, A]): EitherT[F, Error2, A] =
        fa.leftMap(f)
      end apply
    end new
  end mapErrorK

  def runStateT[IO[_] : FlatMap, State](bounds : IO[State]) : StateT[IO, State, *] ~> IO =
    new ~>[StateT[IO, State, *], IO]:
      override def apply[A](fa: StateT[IO, State, A]): IO[A] =
        bounds.flatMap(fa.runA)
      end apply
    end new
  end runStateT

  def flattenEitherTK[F[_] : Functor, Error] : EitherT[EitherT[F, Error, *], Error, *] ~> EitherT[F, Error, *] =
    new ~>[EitherT[EitherT[F, Error, *], Error, *], EitherT[F, Error, *]]:
      override def apply[A](fa: EitherT[EitherT[F, Error, *], Error, A]): EitherT[F, Error, A] =
        EitherT(fa.value.value.map(_.flatten))
      end apply
    end new
  end flattenEitherTK
end functionk
