package me.katze.gui4s.example
package placeable

import cats.Monad

import scala.annotation.tailrec

@FunctionalInterface
trait Placeable[+T]:
  def place(bounds : Bounds) : T

  def map[U](f : T => U) : Placeable[U] =
    bounds => f(place(bounds))
  end map
  
  def flatMap[U](f : T => Placeable[U]) : Placeable[U] =
    bounds => f(place(bounds)).place(bounds)
  end flatMap
end Placeable

given placementIsEffect : Monad[Placeable] with
  override def flatMap[A, B](fa: Placeable[A])
                            (f: A => Placeable[B]): Placeable[B] =
    bounds => f(fa.place(bounds)).place(bounds)
  end flatMap

  override def pure[A](x: A): Placeable[A] = bounds => x

  override def tailRecM[A, B](a: A)(f: A => Placeable[Either[A, B]]): Placeable[B] =
    bounds =>
      @tailrec
      def helper(a: A)(f: A => Placeable[Either[A, B]]) : B =
        f(a).place(bounds) match
          case Left(value) => helper(value)(f)
          case Right(value) => value
        end match
      end helper
      helper(a)(f) 
  end tailRecM
end placementIsEffect
