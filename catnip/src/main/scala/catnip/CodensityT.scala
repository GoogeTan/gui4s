package catnip

import cats.Applicative
import cats.FlatMap
import cats.Monad
import cats.syntax.all.*

final case class CodensityT[F[_], A](f : [T] => (A => F[T]) => F[T]):
  def run[T](k : A => F[T]) : F[T] =
    f[T](k)
  end run

  def extract(using A : Applicative[F]) : F[A] =
    f(A.pure)
  end extract

  def flatMap[B](fa: CodensityT[F, A])(f: A => CodensityT[F, B]): CodensityT[F, B] =
    CodensityT(
      [T] => (ff : B => F[T]) =>
        fa.f((a : A) => f(a).f[T](ff(_)))
    )
  end flatMap

  def map[B](f: A => B): CodensityT[F, B] =
    CodensityT([T] => (ff : B => F[T]) => this.f[T](a => ff(f(a))))
  end map
end CodensityT

object CodensityT:
  def pure[F[_], A](a : A) : CodensityT[F, A] =
    CodensityT([T] => (ff : A => F[T]) => ff(a))
  end pure

  def liftF[F[_], A](fa : F[A])(using F : FlatMap[F]) : CodensityT[F, A] =
    CodensityT([T] => (ff : A => F[T]) => fa.flatMap(ff))
  end liftF

  given codensityIsMonad[IO[_] : Monad as IOM]: Monad[CodensityT[IO, *]] =
    new Monad[CodensityT[IO, *]]:
      override def pure[A](x: A): CodensityT[IO, A] =
        CodensityT[IO, A]([T] => k => k(x))
      end pure

      override def flatMap[A, B](fa: CodensityT[IO, A])(f: A => CodensityT[IO, B]): CodensityT[IO, B] =
        CodensityT(
          [T] => (ff : B => IO[T]) =>
            fa.f((a : A) => f(a).f[T](ff))
        )
      end flatMap

      override def tailRecM[A, B](a: A)(f: A => CodensityT[IO, Either[A, B]]): CodensityT[IO, B] =
        CodensityT(
          [T] => (ff : B => IO[T]) =>
            IOM.tailRecM(a)(
              currentA =>
                f(currentA).f(IOM.pure)
            ).flatMap(ff)
        )
    end new
  end codensityIsMonad
end CodensityT
