package catnip
package syntax

import cats.*
import cats.arrow.FunctionK
import cats.syntax.all.*

object additional:
  given applicativeUnitIsMonoid[F[_] : Applicative as M]: Monoid[F[Unit]] with
    override def empty: F[Unit] =
      M.pure(())
    end empty

    override def combine(x: F[Unit], y: F[Unit]): F[Unit] =
      x *> y
    end combine
  end applicativeUnitIsMonoid


  given InjectMonad[G[_] : Applicative] : InjectK[Id, G] with
    override def inj: FunctionK[Id, G] =
      new FunctionK[Id, G]:
        override def apply[A](value : A) : G[A] = value.pure[G]
      end new
    end inj

    override def prj: FunctionK[G, Option] =
      new FunctionK[G, Option]:
        override def apply[A](value : G[A]) : Option[A] = None
      end new
    end prj
  end InjectMonad
end additional
