package catnip
package syntax

import cats.Monad
import cats.data.{StateT, Writer, WriterT}
import cats.syntax.all.*

object bi:
  given[Trait[G[_]], F[_, _] : Bi[Trait] as b, A] : Trait[F[A, *]] = b[A]()

  given writerIsBiMonad[F[_] : Monad] : BiMonad[[A, B] =>> WriterT[F, List[A], B]] = [A] => () => Monad[WriterT[F, List[A], *]]
  given stateWrapsBiMonad[F[_, _] : BiMonad, S] : BiMonad[[A, B] =>> StateT[F[A, *], S, B]] = [A] => () => Monad[StateT[F[A, *], S, *]]
end bi