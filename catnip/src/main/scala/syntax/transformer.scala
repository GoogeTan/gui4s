package catnip
package syntax

import syntax.additional.*
import syntax.applicative.nestedFunctorsAreFunctors
import syntax.state.given
import catnip.transformer.MonadTransformer

import cats.*
import cats.data.*

object transformer:
  type <>[F[_[_], _], G[_[_], _]] = [IO[_], T] =>> F[G[IO, *], T]

  given monadInstanceForTransformer[F[_[_], _]: MonadTransformer as FMT, IO[_] : Monad] : Monad[F[IO, *]] = FMT.monadInstance[IO]
  
  
  given composedMonadTransformerInstance[F[_[_], _] : MonadTransformer as FMT, U[_[_], _] : MonadTransformer as UMT]: MonadTransformer[F <> U] with
    override given monadInstance[IO[_] : Monad]: Monad[(F <> U)[IO, *]] =
      FMT.monadInstance[U[IO, *]](using UMT.monadInstance[IO])
    end monadInstance

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): (F <> U)[G, *] ~> (F <> U)[K, *] =
      FMT.liftFunctionK(UMT.liftFunctionK(f))
    end liftFunctionK

    override def liftK[G[_] : Monad]: G ~> (F <> U)[G, *] =
      UMT.liftK.andThen(FMT.liftK)
    end liftK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](
                                                                    original: (F <> U)[G, A],
                                                                    f: [Inner[_] : Functor] => G[Inner[A]] => K[Inner[B]]
                                                                  ): (F <> U)[K, B] =
      FMT.innerTransform[U[G, *], U[K, *], A, B](
        original,
        [Inner[_] : Functor] => (value : U[G, Inner[A]]) =>
          UMT.innerTransform[G, K, Inner[A], Inner[B]](value,
            [Inner2[_] : Functor] => (value : G[Inner2[Inner[A]]]) => 
              f[Inner2 * Inner](value)
          )
      )
    end innerTransform
  end composedMonadTransformerInstance
end transformer
