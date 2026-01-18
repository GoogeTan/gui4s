package catnip
package syntax

import catnip.transformer._
import cats._
import cats.data._
import cats.syntax.all._

object transformer:
  type <>[F[_[_], _], G[_[_], _]] = [IO[_], T] =>> F[G[IO, *], T]

  given monadInstanceForTransformer[F[_[_], _]: MonadTransformer as FMT, IO[_] : Monad] : Monad[F[IO, *]] = FMT.monadInstance[IO]
  given monadErrorInstanceForTransformer[F[_[_], _]: MonadTransformer as FMT, IO[_], Error](using M : MonadError[IO, Error]) : MonadError[F[IO, *], Error] with
    val original : Monad[F[IO, *]] = monadInstanceForTransformer[F, IO]
    export original.*

    override def raiseError[A](e: Error): F[IO, A] =
      FMT.liftK(M.raiseError(e))
    end raiseError

    override def handleErrorWith[A](fa: F[IO, A])(f: Error => F[IO, A]): F[IO, A] =
      FMT.monadInstance[IO].flatten(
        FMT.innerTransform[IO, IO, A, F[IO, A]](fa, [Inner[_] : Applicative] => (ioInnerA : IO[Inner[A]]) =>
            M.handleErrorWith[Inner[F[IO, A]]](M.map(ioInnerA)(_.map(pure)))(error => f(error).pure[Inner].pure[IO])
        )
      ) 
    end handleErrorWith
  end monadErrorInstanceForTransformer

  given stateT2Instance[S]: MonadTransformer[[IO[_], T] =>> StateT[IO, S, T]] with
    override def liftK[G[_] : Monad]: G ~> StateT[G, S, *] =
      StateT.liftK[G, S]
    end liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): StateT[G, S, *] ~> StateT[K, S, *] =
      new (StateT[G, S, *] ~> StateT[K, S, *]):
        override def apply[A](fa: StateT[G, S, A]): StateT[K, S, A] =
          StateT[K, S, A](
            state =>
              f(fa.run(state))
          )
    end liftFunctionK

    override def innerTransform[G[_] : Monad as GM, K[_] : Monad as KM, A, B](original: StateT[G, S, A], f: [Inner[_] : Applicative] => G[Inner[A]] => K[Inner[B]]): StateT[K, S, B] =
      StateT(state =>
        given Monoid[S] with
          override def empty: S = state

          override def combine(x: S, y: S): S = state
        end given
        KM.map(
          f[Writer[S, *]](GM.map(original.run(state))(Writer(_, _)))
        )(_.run)
      )
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[StateT[IO, S, *]] = summon
  end stateT2Instance

  given readerTInstance[C]: MonadTransformer[ReaderTransformer[C]] with
    override def liftK[G[_] : Monad]: G ~> ReaderT[G, C, *] =
      ReaderT.liftK[G, C]
    end liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): ReaderT[G, C, *] ~> ReaderT[K, C, *] =
      ReaderT.liftFunctionK(f)
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: ReaderT[G, C, A], f: [Inner[_] : Applicative] => G[Inner[A]] => K[Inner[B]]): ReaderT[K, C, B] =
      ReaderT(source => f[Id](original.run(source)))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[ReaderT[IO, C, *]] = summon
  end readerTInstance

  given writerTInstance[L: Monoid]: MonadTransformer[WriterTransformer[L]] with
    override def liftK[G[_] : Monad]: G ~> WriterT[G, L, *] = WriterT.liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): WriterT[G, L, *] ~> WriterT[K, L, *] =
      WriterT.liftFunctionK(f)
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: WriterT[G, L, A], f: [Inner[_] : Applicative] => G[Inner[A]] => K[Inner[B]]): WriterT[K, L, B] =
      WriterT(f[(L, *)](original.run))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[WriterT[IO, L, *]] = summon
  end writerTInstance

  given eitherTInstance[Error]: MonadTransformer[[IO[_], T] =>> EitherT[IO, Error, T]] with
    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): EitherT[G, Error, *] ~> EitherT[K, Error, *] =
      new~>[EitherT[G, Error, *], EitherT[K, Error, *]]:
        override def apply[A](fa: EitherT[G, Error, A]): EitherT[K, Error, A] =
          EitherT(f(fa.value))
        end apply
      end new
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: EitherT[G, Error, A], f: [Inner[_] : Applicative] => G[Inner[A]] => K[Inner[B]]): EitherT[K, Error, B] =
      EitherT(f[Either[Error, *]](original.value))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[EitherT[IO, Error, *]] = summon

    override def liftK[G[_] : Monad]: G ~> EitherT[G, Error, *] = EitherT.liftK
  end eitherTInstance

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

    override def innerTransform[G[_] : Monad as GM, K[_] : Monad as KM, A, B](
                                                                    original: (F <> U)[G, A],
                                                                    f: [Inner[_] : Applicative] => G[Inner[A]] => K[Inner[B]]
                                                                  ): (F <> U)[K, B] =
      FMT.innerTransform[U[G, *], U[K, *], A, B](
        original,
        [Inner[_] : Applicative] => (value : U[G, Inner[A]]) =>
          UMT.innerTransform[G, K, Inner[A], Inner[B]](value,
            [Inner2[_] : Applicative] => (value : G[Inner2[Inner[A]]]) => 
              KM.map(f[Nested[Inner2, Inner, *]](GM.map(value)(Nested(_))))(_.value)
          )
      )
    end innerTransform
  end composedMonadTransformerInstance
end transformer
