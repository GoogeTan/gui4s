package catnip
package syntax

import catnip.resource.{Allocate, Eval, EvalC, Make}
import cats.effect.{IO, LiftIO, MonadCancel, Resource}
import cats.syntax.all.*
import cats.{FlatMap, Functor, ~>}

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
    override def evalK: F ~> Resource[F, *] =
      Resource.liftK
    end evalK
  end eval

  given liftEval[F[_] : LiftIO as L]: Eval[Resource[F, *], IO] with
    override def evalK: IO ~> Resource[F, *] =
      new ~>[IO, Resource[F, *]]:
        override def apply[A](fa: IO[A]): Resource[F, A] =
          Resource.eval(L.liftIO(fa))
        end apply
      end new
    end evalK
  end liftEval
end resource
