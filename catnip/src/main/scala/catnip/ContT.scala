package catnip

import cats.Applicative
import cats.FlatMap
import cats.Monad
import cats.syntax.all._

final case class ContT[F[_], A](f : [T] => (A => F[T]) => F[T]):
  def run[T](k : A => F[T]) : F[T] =
    f[T](k)
  end run

  def extract(using A : Applicative[F]) : F[A] =
    f(A.pure)
  end extract

  def flatMap[B](fa: ContT[F, A])(f: A => ContT[F, B]): ContT[F, B] =
    ContT(
      [T] => (ff : B => F[T]) =>
        fa.f((a : A) => f(a).f[T](ff(_)))
    )
  end flatMap

  def map[B](f: A => B): ContT[F, B] =
    ContT([T] => (ff : B => F[T]) => this.f[T](a => ff(f(a))))
  end map
end ContT

object ContT:
  def pure[F[_], A](a : A) : ContT[F, A] =
    ContT([T] => (ff : A => F[T]) => ff(a))
  end pure

  def liftF[F[_], A](fa : F[A])(using F : FlatMap[F]) : ContT[F, A] =
    ContT([T] => (ff : A => F[T]) => fa.flatMap(ff))
  end liftF

  given contTisMonad[IO[_] : Monad as IOM]: Monad[ContT[IO, *]] =
    new Monad[ContT[IO, *]]:
      override def pure[A](x: A): ContT[IO, A] =
        ContT[IO, A]([T] => k => k(x))
      end pure

      override def flatMap[A, B](fa: ContT[IO, A])(f: A => ContT[IO, B]): ContT[IO, B] =
        ContT(
          [T] => (ff : B => IO[T]) =>
            fa.f((a : A) => f(a).f[T](ff(_)))
        )
      end flatMap

      override def tailRecM[A, B](a: A)(f: A => ContT[IO, Either[A, B]]): ContT[IO, B] =
        ContT(
          [T] => (ff : B => IO[T]) =>
            IOM.tailRecM(a)(
              currentA =>
                f(a).f(IOM.pure)
            ).flatMap(ff)
        )
    end new
  end contTisMonad
end ContT





