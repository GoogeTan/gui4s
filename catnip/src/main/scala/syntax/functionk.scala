package catnip
package syntax

import cats.data.{EitherT, StateT}
import cats.syntax.all.*
import cats.{FlatMap, Functor, MonadError, ~>}

object functionk:
  given[F[_], G[_]]: Conversion[F ~> G, [T] => F[T] => G[T]] =
    f => [T] => a => f(a)

  given[F[_], G[_]]: Conversion[[T] => F[T] => G[T], F ~> G] =
    (f : [T] => F[T] => G[T]) =>
      new ~>[F, G]:
        override def apply[A](fa: F[A]): G[A] =
          f(fa)
        end apply
      end new
  end given

  def runEitherT[F[_], Error](using AE: MonadError[F, Error]) : EitherT[F, Error, *] ~> F =
    new ~>[EitherT[F, Error, *], F]:
      override def apply[A](fa: EitherT[F, Error, A]): F[A] =
        fa.value.flatMap(AE.fromEither)
      end apply
    end new
  end runEitherT

  def eitherTMapError[F[_] : Functor, Error1, Error2](f : Error1 => Error2) : EitherT[F, Error1, *] ~> EitherT[F, Error2, *] =
    new ~>[EitherT[F, Error1, *], EitherT[F, Error2, *]]:
      override def apply[A](fa: EitherT[F, Error1, A]): EitherT[F, Error2, A] =
        fa.leftMap(f)
      end apply
    end new
  end eitherTMapError

  def runStateT[IO[_] : FlatMap, State](bounds : IO[State]) : StateT[IO, State, *] ~> IO =
    new ~>[StateT[IO, State, *], IO]:
      override def apply[A](fa: StateT[IO, State, *][A]): IO[A] =
        bounds.flatMap(fa.runA)
      end apply
    end new
  end runStateT
end functionk
