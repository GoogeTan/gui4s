package catnip
package syntax

import cats.Applicative
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
end applicative
