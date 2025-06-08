package catnip
package syntax

object bi:
  given[Trait[G[_]], F[_, _] : Bi[Trait] as b, A] : Trait[F[A, *]] = b[A]()