package catnip
package syntax

object bi:
  given[Trait[F[_]], F[_, _] : Bi[Trait] as b, A] : Trait[F[A, *]] = b[A]()
