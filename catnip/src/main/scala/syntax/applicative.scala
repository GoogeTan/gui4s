package catnip
package syntax

import cats.{Applicative, Functor}
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
  
  given [F[_] : Functor, G[_] : Functor] : Functor[[Value] =>> F[G[Value]]] with
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      fa.map(_.map(f))
    end map
  end given
end applicative
