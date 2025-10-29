package catnip
package syntax

object additional:
  type *[F[_], G[_]] = [Value] =>> F[G[Value]]
end additional
