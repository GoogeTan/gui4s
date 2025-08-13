package catnip
package syntax

import cats.data.EitherT
import cats.syntax.all.{*, given}
import cats.{MonadError, ~>}

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

  def liftRaiseError[F[_], Error](using AE: MonadError[F, Error]) : EitherT[F, Error, *] ~> F =
    new ~>[EitherT[F, Error, *], F]:
      override def apply[A](fa: EitherT[F, Error, A]): F[A] =
        fa.value.flatMap(AE.fromEither)
      end apply
    end new
  end liftRaiseError
end functionk
