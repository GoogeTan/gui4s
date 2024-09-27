package me.katze.gui4s.widget
package placeable

import cats.Monad

import scala.annotation.tailrec

@FunctionalInterface
trait Placeable[-Bounds, +T]:
  def place(bounds : Bounds) : T
end Placeable

given placementIsEffect[Bounds] : Monad[[T] =>> Placeable[Bounds, T]] with
  override def flatMap[A, B](fa: Placeable[Bounds, A])
                            (f: A => Placeable[Bounds, B]): Placeable[Bounds, B] =
    bounds => f(fa.place(bounds)).place(bounds)
  end flatMap

  override def pure[A](x: A): Placeable[Bounds, A] = * => x

  override def tailRecM[A, B](a: A)(f: A => Placeable[Bounds, Either[A, B]]): Placeable[Bounds, B] =
    bounds =>
      @tailrec
      def helper(a: A)(f: A => Placeable[Bounds, Either[A, B]]) : B =
        f(a).place(bounds) match
          case Left(value) => helper(value)(f)
          case Right(value) => value
        end match
      end helper
      helper(a)(f) 
  end tailRecM
end placementIsEffect
