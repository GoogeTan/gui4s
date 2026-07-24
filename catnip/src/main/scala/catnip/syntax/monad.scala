package catnip
package syntax

import cats.{FlatMap, Functor, Monad, MonadError}
import cats.effect.*
import cats.syntax.all.*

object monad:
  /**
   * Каррированная версия MonadError.
   */
  type MonadErrorC[Error] = [F[_]] =>> MonadError[F, Error]
  type MonadCancelC[Error] = [F[_]] =>> MonadCancel[F, Error]
  
  export CodensityT.codensityIsMonad

  extension[F[_] : FlatMap as FM, A](value : F[A])
    def <*<[B](f : A => F[B]) : F[A] =
      FM.flatTap(value)(f)
    end <*<

    def sigmaFlatProduct[B](f: A => F[B]): F[(A, B)] =
      for
        a <- value
        b <- f(a)
      yield (a, b)
    end sigmaFlatProduct
  end extension

  extension[F[_]: Functor, A] (value: F[A])
    def sigmaProduct[B](f: A => B): F[(A, B)] =
      for
        a <- value
        b = f(a)
      yield (a, b)
    end sigmaProduct
  end extension
end monad
