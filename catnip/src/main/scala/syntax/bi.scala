package catnip
package syntax

import cats.Monad
import cats.data.{StateT, WriterT, EitherT}

object bi:
  given unwrapBi[Trait[G[_]], F[_, _] : Bi[Trait] as b, A] : Trait[F[A, *]] = b[A]()
end bi
