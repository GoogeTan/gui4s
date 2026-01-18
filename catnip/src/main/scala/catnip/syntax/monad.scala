package catnip
package syntax

import cats.FlatMap
import cats.MonadError
import cats.effect._
import cats.syntax.all._

object monad:
  /**
   * Каррированная версия MonadError.
   */
  type MonadErrorC[Error] = [F[_]] =>> MonadError[F, Error]
  type MonadCancelC[Error] = [F[_]] =>> MonadCancel[F, Error]
  
  export ContT.contTisMonad

  extension[F[_] : FlatMap as FM, A](value : F[A])
    def <*<[B](f : A => F[B]) : F[A] =
      FM.flatTap(value)(f)
    end <*<

    def sigmaProduct[B](f: A => F[B]): F[(A, B)] =
      for
        a <- value
        b <- f(a)
      yield (a, b)
    end sigmaProduct
  end extension
end monad
