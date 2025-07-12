package catnip
package syntax

import cats.Monad
import cats.data.{Writer, WriterT}
import cats.syntax.all.*

object bi:
  given[Trait[G[_]], F[_, _] : Bi[Trait] as b, A] : Trait[F[A, *]] = b[A]()

  given writerIsBiMonad : BiMonad[[A, B] =>> Writer[List[A], B]] = [A] => () => Monad[Writer[List[A], *]]
end bi