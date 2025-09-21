package catnip
package syntax

import cats.FlatMap
import cats.effect.Resource

object resource:
  extension [F[_] : FlatMap as FM, A](value: Resource[F, A])
    def <*<[B](f: A => F[B]): Resource[F, A] =
      value.evalTap(f)
    end <*<
  end extension
end resource
