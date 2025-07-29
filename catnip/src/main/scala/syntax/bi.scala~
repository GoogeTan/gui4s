package catnip
package syntax

import cats.Monad
import cats.data.{StateT, Writer, WriterT, EitherT}
import cats.syntax.all.*

object bi:
  given[Trait[G[_]], F[_, _] : Bi[Trait] as b, A] : Trait[F[A, *]] = b[A]()

  given writerIsBiMonad[F[_] : Monad] : BiMonad[[A, B] =>> WriterT[F, List[A], B]] = [A] => () => Monad[WriterT[F, List[A], *]]
  given stateWrapsBiMonad[F[_, _] : BiMonad, S] : BiMonad[[A, B] =>> StateT[F[A, *], S, B]] = [A] => () => Monad[StateT[F[A, *], S, *]]
  given eitherWrapsBiMonad[F[_, _] : BiMonad, Error] : BiMonad[[A, B] =>> EitherT[F[A, *], Error, B]] = [A] => () => Monad[EitherT[F[A, *], Error, *]]
end bi