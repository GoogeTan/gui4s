package catnip
package transformer

import syntax.additional.*

import cats.data.{EitherT, WriterT}
import cats.{Functor, Monad, Monoid, ~>}

object instances:
  given stateTInstance[S] : MonadTransformer[[IO[_], T] =>> MyStateT[IO, S, T]] with
    override def liftK[G[_] : Monad]: G ~> MyStateT[G, S, *] =
      MyStateT.liftK[G, S]
    end liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): MyStateT[G, S, *] ~> MyStateT[K, S, *] =
      MyStateT.liftFunctionK(f)
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: MyStateT[G, S, A], f: [Inner[_] : Functor] => G[Inner[A]] => K[Inner[B]]): MyStateT[K, S, B] =
      MyStateT(state => f[(S, *)](original.run(state)))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[MyStateT[IO, S, *]] = summon
  end stateTInstance

  given writerTInstance[L: Monoid]: MonadTransformer[[IO[_], T] =>> WriterT[IO, L, T]] with
    override def liftK[G[_] : Monad]: G ~> WriterT[G, L, *] = WriterT.liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): WriterT[G, L, *] ~> WriterT[K, L, *] =
      WriterT.liftFunctionK(f)
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: WriterT[G, L, A], f: [Inner[_] : Functor] => G[Inner[A]] => K[Inner[B]]): WriterT[K, L, B] =
      WriterT(f[(L, *)](original.run))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[WriterT[IO, L, *]] = summon
  end writerTInstance

  given eitherTInstance[Error] : MonadTransformer[[IO[_], T] =>> EitherT[IO, Error, T]] with
    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): EitherT[G, Error, *] ~> EitherT[K, Error, *] =
      new ~>[EitherT[G, Error, *], EitherT[K, Error, *]]:
        override def apply[A](fa: EitherT[G, Error, A]): EitherT[K, Error, A] =
          EitherT(f(fa.value))
        end apply
      end new
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: EitherT[G, Error, A], f: [Inner[_] : Functor] => G[Inner[A]] => K[Inner[B]]): EitherT[K, Error, B] =
      EitherT(f[Either[Error, *]](original.value))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[EitherT[IO, Error, *]] = summon

    override def liftK[G[_] : Monad]: G ~> EitherT[G, Error, *] = EitherT.liftK
  end eitherTInstance
end instances