package catnip
package syntax

import cats.Monad

import scala.annotation.targetName

object bimonad:
  given biMonadIsMonad[F[+_, +_] : BiMonad as BM, T]: Monad[[A] =>> F[A, T]] with
    override def flatMap[A, B](fa: F[A, T])(f: A => F[B, T]): F[B, T] =
      BM.flatMapFirst(fa)(f)
    end flatMap

    override def pure[A](x: A): F[A, T] =
      BM.pure(x)
    end pure

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B], T]): F[B, T] =
      BM.tailRecM(a)(f)
    end tailRecM
  end biMonadIsMonad

  extension [F[_, +_] : BiMonad as BM, A, B](value: F[A, B])
    def map[C](f: A => C): F[C, B] = BM.flatMapFirst(value)(a => BM.pure(f(a)))
    def mapSecond[D](f: B => D): F[A, D] = BM.mapSecond(value)(f)
    def flatMap[C](f: A => F[C, B]): F[C, B] = BM.flatMapFirst(value)(f)
    @targetName("aliasForFlatMap")
    def >>=[C](f: A => F[C, B]): F[C, B] = flatMap(f)

    def *>[C](newValue: F[C, B]): F[C, B] = flatMap(_ => newValue)
    def <*[C](newValue: F[C, B]): F[A, B] = flatMap(a => newValue.map(_ => a))
  end extension
  
  extension [F[_, +_] : BiMonad as BM, A](value : A)
    def pure : F[A, Nothing] = 
      BM.pure(value)
    end pure
  end extension
end bimonad
