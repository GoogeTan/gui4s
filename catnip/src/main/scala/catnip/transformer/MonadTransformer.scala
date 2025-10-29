package catnip
package transformer

import cats.*

trait MonadTransformer[F[_[_], _]]:
  given monadInstance[IO[_] : Monad]: Monad[F[IO, *]] = scala.compiletime.deferred

  def liftK[G[_] : Monad] : G ~> F[G, *]
  def liftFunctionK[G[_] : Monad, K[_] : Monad](f : G ~> K) : F[G, *] ~> F[K, *]

  def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: F[G, A], f: [Inner[_] : Applicative] => G[Inner[A]] => K[Inner[B]]): F[K, B]
end MonadTransformer

object MonadTransformer:
  def apply[F[_[_], _]](using m : MonadTransformer[F]): MonadTransformer[F] =
    m
  end apply
end MonadTransformer
