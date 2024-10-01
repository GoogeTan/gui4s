package me.katze.gui4s.layout

import bound.Bounds

import cats.{FlatMap, Monad}
import io.github.iltotore.iron.{*, given}

import scala.annotation.tailrec

type MeasurableT[MU] = [T] =>> Measurable[MU, T]

trait Measurable[MU, +T]:
  def placeInside(constrains: Bounds[MU]): Sized[MU, T]
end Measurable

given[MU]: FlatMap[MeasurableT[MU]] with
  override def flatMap[A, B](fa: Measurable[MU, A])
                            (f: A => Measurable[MU, B]): Measurable[MU, B] =
    (constrains: Bounds[MU]) => f(fa.placeInside(constrains).value).placeInside(constrains)

  override def map[A, B](fa: Measurable[MU, A])(f: A => B): Measurable[MU, B] = c =>
    val placed = fa.placeInside(c)
    Sized(f(placed.value), placed.width, placed.height)
  end map

  override def tailRecM[A, B](a: A)(f: A => Measurable[MU, Either[A, B]]): Measurable[MU, B] =
    c =>
      @tailrec
      def helper(a : A)(f: A => Measurable[MU, Either[A, B]]): Sized[MU, B] =
        f(a).placeInside(c) match
          case Sized(Left(a), _, _) => helper(a)(f)
          case Sized(Right(value), w, h) => Sized(value, w, h)
        end match
      end helper

      helper(a)(f)
  end tailRecM
end given