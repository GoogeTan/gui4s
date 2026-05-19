package catnip

import catnip.transformer.IncrementalStateT
import cats.*
import cats.syntax.all.*

type Incremental[F[_], G[_], T] = IncrementalStateT[F, G, T, Unit]

object Incremental:
  def apply[F[_] : Functor, G[_], T](f : T => F[G[T]]) : Incremental[F, G, T] =
    IncrementalStateT.modify(f)
  end apply

  given[F[_] : Monad, G[_] : Patch, T] : Monoid[Incremental[F, G, T]] =
    Monoid.instance(
      IncrementalStateT.monadInstance.pure(()),
      _ *> _
    )
  end given
end Incremental
