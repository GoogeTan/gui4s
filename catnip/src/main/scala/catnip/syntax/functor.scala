package catnip.syntax

import cats.Functor
import cats.syntax.functor.*

object functor:
  class nestedFunctorsAreFunctors[F[_] : Functor, G[_] : Functor] extends Functor[[Value] =>> F[G[Value]]]:
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      fa.map(_.map(f))
    end map
  end nestedFunctorsAreFunctors
end functor
