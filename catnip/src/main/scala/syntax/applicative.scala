package catnip
package syntax

import cats.{Applicative, ApplicativeError, Functor}
import cats.kernel.Monoid
import cats.syntax.all.*

object applicative:
  given applicativesAreMonoids[F[_] : Applicative as A] : Monoid[F[Unit]] with
    override def empty: F[Unit] =
      A.pure(())
    end empty

    override def combine(x: F[Unit], y: F[Unit]): F[Unit] =
      x *> y
    end combine
  end applicativesAreMonoids
  
  given nestedFunctorsAreFunctors[F[_] : Functor, G[_] : Functor] : Functor[[Value] =>> F[G[Value]]] with
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      fa.map(_.map(f))
    end map
  end nestedFunctorsAreFunctors
  
  extension[T](value : Option[T])
    def getOrRaise[F[_], Error](using A : ApplicativeError[F, Error])(error : Error) : F[T] =
      value.map(_.pure[F]).getOrElse(A.raiseError(error))
    end getOrRaise
  end extension
end applicative
