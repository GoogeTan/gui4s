package catnip
package syntax

import catnip.resource.*
import catnip.syntax.transformer.given
import catnip.transformer.MonadTransformer
import cats.data.*
import cats.effect.{IO, LiftIO, MonadCancel, Resource}
import cats.syntax.all.*
import cats.{Applicative, FlatMap, Functor, Monad, Semigroup, ~>}

object resource:
  extension [IO[_], Resource[_] : {FlatMap, EvalC[IO]}, A](value: Resource[A])
    def <*<[B](f: A => IO[B]): Resource[A] =
      value.flatTap(f.andThen(_.eval))
    end <*<
  end extension

  given make[F[_] : Functor] : Make[Resource[F, *], F] with
    override def make[T](value: F[(T, F[Unit])]): Resource[F, T] =
      Resource(value)
    end make
  end make

  given liftMake[F[_] : {LiftIO as L, Functor}] : Make[Resource[F, *], IO] with
    override def make[T](value: IO[(T, IO[Unit])]): Resource[F, T] =
      Resource(L.liftIO(value.map((value, destructor) => (value, L.liftIO(destructor)))))
    end make
  end liftMake

  given allocate[F[_]](using MonadCancel[F, Throwable]) : Allocate[Resource[F, *], F] with
    override def allocate[T](value: Resource[F, T]): F[(T, F[Unit])] =
      value.allocated
    end allocate
  end allocate

  given eval[F[_]] : Eval[Resource[F, *], F] with
    override def liftToResource: F ~> Resource[F, *] =
      Resource.liftK
    end liftToResource
  end eval

  given evalRMT[
    Resource[_] : {Monad, EvalC[F] as E},
    F[_],
    MT[_[_], _] : MonadTransformer as MT
  ]: Eval[MT[Resource, *], F] with
    override def liftToResource: F ~> MT[Resource, *] =
      E.liftToResource.andThen(MT.liftK)
    end liftToResource
  end evalRMT

  given evalMT[
    Resource[_] : {Monad, EvalC[F] as E},
    F[_],
    MT[_[_], _] : MonadTransformer as MT
  ](using Monad[F]): Eval[MT[Resource, *], MT[F, *]] with
    override def liftToResource: MT[F, *] ~> MT[Resource, *] =
      MT.liftFunctionK(E.liftToResource)
    end liftToResource
  end evalMT

  given liftEval[F[_] : LiftIO as L]: Eval[Resource[F, *], IO] with
    override def liftToResource: IO ~> Resource[F, *] =
      new ~>[IO, Resource[F, *]]:
        override def apply[A](fa: IO[A]): Resource[F, A] =
          Resource.eval(L.liftIO(fa))
        end apply
      end new
    end liftToResource
  end liftEval

  given use[IO[_]](using MonadCancel[IO, Throwable]) : Use[Resource[IO, *], IO] with
    override def useResource[T, B](value: Resource[IO, T])(f: T => IO[B]): IO[B] =
      value.use(f)
    end useResource
  end use

  given useOptionT[
    IO[_],
    Resource[_]
  ](
    using use: Use[Resource, IO]
  )(
    using Monad[Resource], MonadCancel[IO, Throwable]
  ) : Use[OptionT[Resource, *], OptionT[IO, *]] with
    override def useResource[T, B](value: OptionT[Resource, T])(f: T => OptionT[IO, B]): OptionT[IO, B] =
      OptionT(
        value.value.use:
          case None => None.pure
          case Some(t) =>
            f(t).value
      )
    end useResource
  end useOptionT

  given useEitherT[
    IO[_] : Applicative,
    Resource[_],
    E
  ](
    using use: Use[Resource, IO]
  ): Use[EitherT[Resource, E, *], EitherT[IO, E, *]] with
    override def useResource[T, B](value: EitherT[Resource, E, T])(f: T => EitherT[IO, E, B]): EitherT[IO, E, B] =
      EitherT(
        value.value.use:
          case Left(e) => e.asLeft[B].pure[IO]
          case Right(t) => f(t).value
      )
    end useResource
  end useEitherT

  given useStateT[
    IO[_] : Monad,
    Resource[_] : FlatMap,
    S
  ](
    using use: Use[Resource, IO]
  ): Use[StateT[Resource, S, *], StateT[IO, S, *]] with

    override def useResource[T, B](value: StateT[Resource, S, T])(f: T => StateT[IO, S, B]): StateT[IO, S, B] =
      StateT { (s: S) =>
        value.run(s).use:
          case (sNext, t) => f(t).run(sNext)
      }
    end useResource

  end useStateT

  given useWriterT[
    IO[_],
    Resource[_],
    L
  ](
    using use: Use[Resource, IO]
  )(
    using Functor[IO], Semigroup[L]
  ): Use[WriterT[Resource, L, *], WriterT[IO, L, *]] with

    override def useResource[T, B](value: WriterT[Resource, L, T])(f: T => WriterT[IO, L, B]): WriterT[IO, L, B] =
      WriterT(
        value.run.use:
          case (l1, t) =>
            f(t).run.map:
              case (l2, b) => (l1 |+| l2, b)
      )
    end useResource

  end useWriterT

  given useReaderT[
    IO[_],
    Resource[_],
    R
  ](
    using use: Use[Resource, IO]
  ): Use[ReaderT[Resource, R, *], ReaderT[IO, R, *]] with

    override def useResource[T, B](value: ReaderT[Resource, R, T])(f: T => ReaderT[IO, R, B]): ReaderT[IO, R, B] =
      ReaderT { (r: R) =>
        value.run(r).use { t =>
          f(t).run(r)
        }
      }
    end useResource

  end useReaderT

  export SyncResource.given
end resource
