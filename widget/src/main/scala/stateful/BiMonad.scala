package me.katze.gui4s.widget
package stateful

import cats.Monad

import scala.annotation.targetName

trait BiMonad[F[+_, +_]]:
  def flatMap_[A, B, C](value : F[A, B])(f: A => F[C, B]): F[C, B]
  def tailRecM[A, B, E](a: A)(f: A => F[Either[A, B], E]): F[B, E]
  
  extension[A, B] (value : F[A, B])
    def map[C](f : A => C) : F[C, B] = flatMap(f andThen asMonad)
    def flatMap[C](f : A => F[C, B]) : F[C, B] = flatMap_(value)(f)
    @targetName("aliasForFlatMap")
    def >>=[C](f: A => F[C, B]) : F[C, B] = flatMap(f)
  end extension

  extension[A](value : A)
    // TODO Исправить имя, чтобы не колапсилось с кошачьим
    def asMonad : F[A, Nothing]
  end extension
end BiMonad

object BiMonad:
  def apply[F[+_, +_]](using a : BiMonad[F]): BiMonad[F] = a
end BiMonad

given biMonadIsMonad[F[+_, +_]: BiMonad, T] : Monad[[A] =>> F[A, T]] with
  override def flatMap[A, B](fa: F[A, T])(f: A => F[B, T]): F[B, T] =
    summon[BiMonad[F]].flatMap_(fa)(f)
  end flatMap

  override def pure[A](x: A): F[A, T] =
    x.asMonad
  end pure

  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B], T]): F[B, T] =
    summon[BiMonad[F]].tailRecM(a)(f)
  end tailRecM
end biMonadIsMonad
