package catnip
package syntax

import cats.Applicative
import cats.data.EitherT
import cats.syntax.all.*

object fail:
  extension [F[_, _], T, E1](value : F[E1, T])(using F : FailsWith[F])
    def mapError[E2](f : E1 => E2) : F[E2, T] =
      F.mapError(value)(f)
    end mapError
  end extension

  def failWith[F[_, _], T, E](e : E)(using F : FailsWith[F]) : F[E, T] =
    F.failWith(e)
  end failWith

  given EitherTFails[F[_] : Applicative] : FailsWith[[Error, Value] =>> EitherT[F, Error, Value]] with
    override def mapError[T, E1, E2](fa: EitherT[F, E1, T])(f: E1 => E2): EitherT[F, E2, T] =
      fa.leftMap(f)
    end mapError

    override def failWith[T, E](e: E): EitherT[F, E, T] =
      EitherT.left(e.pure[F])
    end failWith
  end EitherTFails
end fail
