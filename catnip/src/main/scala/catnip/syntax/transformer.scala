package catnip
package syntax

import catnip.transformer._
import cats._
import cats.data._
import cats.syntax.all._

object transformer:
  type <>[F[_[_], _], G[_[_], _]] = [IO[_], T] =>> F[G[IO, *], T]

  given monadInstanceForTransformer[F[_[_], _] : MonadTransformer as FMT, IO[_] : Monad]: Monad[F[IO, *]] = FMT.monadInstance[IO]

  given monadErrorInstanceForTransformer[F[_[_], _], IO[_] : Monad, Inner[_] : Applicative](
    using FMT : MonadTransformer[F], M : MonadError[IO, Error], K : InnerTransform[F, Inner]
  ): MonadError[F[IO, *], Error] with
    val original : Monad[F[IO, *]] = monadInstanceForTransformer[F, IO]
    export original.*

    override def raiseError[A](e: Error): F[IO, A] =
      FMT.liftK(M.raiseError(e))
    end raiseError

    override def handleErrorWith[A](fa: F[IO, A])(f: Error => F[IO, A]): F[IO, A] =
      FMT.monadInstance[IO].flatten(
        K.innerTransform[IO, IO, A, F[IO, A]](fa, (ioInnerA : IO[Inner[A]]) =>
            M.handleErrorWith[Inner[F[IO, A]]](
              M.map(ioInnerA)(_.map(pure))
            )(error =>
              f(error).pure[Inner].pure[IO]
            )
        )
      ) 
    end handleErrorWith
  end monadErrorInstanceForTransformer

  given stateT2Instance[S]: MonadTransformer[[IO[_], T] =>> StateT[IO, S, T]] with InnerTransform[[IO[_], T] =>> StateT[IO, S, T], (S, *)] with
    override def liftK[G[_] : Monad]: G ~> StateT[G, S, *] =
      StateT.liftK[G, S]
    end liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): StateT[G, S, *] ~> StateT[K, S, *] =
      new (StateT[G, S, *] ~> StateT[K, S, *]):
        override def apply[A](fa: StateT[G, S, A]): StateT[K, S, A] =
          StateT[K, S, A](state => f(fa.run(state)))
        end apply
      end new
    end liftFunctionK

    override def innerTransform[G[_] : Monad as GM, K[_] : Monad as KM, A, B](original: StateT[G, S, A], f: G[(S, A)] => K[(S, B)]): StateT[K, S, B] =
      StateT(state => f(original.run(state)))
    end innerTransform
    
    override def monadInstance[IO[_] : Monad]: Monad[StateT[IO, S, *]] = summon
  end stateT2Instance

  given readerTInstance[C]: MonadTransformer[ReaderTransformer[C]] with InnerTransform[ReaderTransformer[C], Id] with
    override def liftK[G[_] : Monad]: G ~> ReaderT[G, C, *] =
      ReaderT.liftK[G, C]
    end liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): ReaderT[G, C, *] ~> ReaderT[K, C, *] =
      ReaderT.liftFunctionK(f)
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: ReaderT[G, C, A], f: G[A] => K[B]): ReaderT[K, C, B] =
      ReaderT(source => f(original.run(source)))
    end innerTransform

    override def monadInstance[IO[_] : Monad]: Monad[ReaderT[IO, C, *]] = summon
  end readerTInstance

  given writerTInstance[L: Monoid as LM]: MonadTransformer[WriterTransformer[L]] with InnerTransform[WriterTransformer[L], (L, *)] with
    override def liftK[G[_] : Monad]: G ~> WriterT[G, L, *] = WriterT.liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): WriterT[G, L, *] ~> WriterT[K, L, *] =
      WriterT.liftFunctionK(f)
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: WriterT[G, L, A], f: G[(L, A)] => K[(L, B)]): WriterT[K, L, B] =
      WriterT(f(original.run))
    end innerTransform
    
    override def monadInstance[IO[_] : Monad]: Monad[WriterT[IO, L, *]] = summon
  end writerTInstance

  given eitherTInstance[Error]: MonadTransformer[[IO[_], T] =>> EitherT[IO, Error, T]] with InnerTransform[[IO[_], T] =>> EitherT[IO, Error, T], Either[Error, *]] with
    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): EitherT[G, Error, *] ~> EitherT[K, Error, *] =
      new ~>[EitherT[G, Error, *], EitherT[K, Error, *]]:
        override def apply[A](fa: EitherT[G, Error, A]): EitherT[K, Error, A] =
          EitherT(f(fa.value))
        end apply
      end new
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](original: EitherT[G, Error, A], f: G[Either[Error, A]] => K[Either[Error, B]]): EitherT[K, Error, B] =
      EitherT(f(original.value))

    override def monadInstance[IO[_] : Monad]: Monad[EitherT[IO, Error, *]] = summon

    override def liftK[G[_] : Monad]: G ~> EitherT[G, Error, *] = EitherT.liftK
  end eitherTInstance

  given composedMonadTransformerInstance[F[_[_], _], U[_[_], _]](
    using FMT : MonadTransformer[F], UMT : MonadTransformer[U]
  ): MonadTransformer[F <> U] with
    override given monadInstance[IO[_] : Monad]: Monad[(F <> U)[IO, *]] =
      FMT.monadInstance[U[IO, *]](using UMT.monadInstance[IO])
    end monadInstance

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): (F <> U)[G, *] ~> (F <> U)[K, *] =
      FMT.liftFunctionK(UMT.liftFunctionK(f))
    end liftFunctionK

    override def liftK[G[_] : Monad]: G ~> (F <> U)[G, *] =
      UMT.liftK.andThen(FMT.liftK)
    end liftK
  end composedMonadTransformerInstance

  given composedMonadTransControl[
    MT1[_[_], _]: MonadTransformer,
    MT2[_[_], _]: MonadTransformer,
    Inner1[_],
    Inner2[_]
  ](using
    MTC1: InnerTransform[MT1, Inner1],
    MTC2: InnerTransform[MT2, Inner2]
  ): InnerTransform[MT1 <> MT2, [X] =>> Inner2[Inner1[X]]] with
    override def innerTransform[G[_]: Monad, K[_]: Monad, A, B](
      original: (MT1 <> MT2)[G, A],
      f: G[Inner2[Inner1[A]]] => K[Inner2[Inner1[B]]]
    ): (MT1 <> MT2)[K, B] =
      MTC1.innerTransform[MT2[G, *], MT2[K, *], A, B](
        original,
        (m2: MT2[G, Inner1[A]]) =>
          MTC2.innerTransform[G, K, Inner1[A], Inner1[B]](m2, f)
      )
  end composedMonadTransControl

  given optionTransformerInstance : InnerTransform[OptionTransformer, Option] with MonadTransformer[OptionTransformer] with
    override def monadInstance[IO[_] : Monad]: Monad[OptionTransformer[IO, *]] =
      summon
    end monadInstance

    override def liftK[G[_] : Monad]: G ~> OptionT[G, *] = OptionT.liftK

    override def liftFunctionK[G[_] : Monad, K[_] : Monad](f: G ~> K): OptionT[G, *] ~> OptionT[K, *] =
      new~>[OptionT[G, *], OptionT[K, *]]:
        override def apply[A](fa: OptionT[G, A]): OptionT[K, A] =
          OptionT(f(fa.value))
        end apply
      end new
    end liftFunctionK

    override def innerTransform[G[_] : Monad, K[_] : Monad, A, B](
      original: OptionTransformer[G, A],
      f: G[Option[A]] => K[Option[B]]
    ): OptionTransformer[K, B] =
      OptionT(f(original.value))
    end innerTransform
  end optionTransformerInstance
end transformer
