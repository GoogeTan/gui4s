package me.katze.gui4s.layout

import bound.Bounds

import cats.FlatMap

import scala.annotation.tailrec

type MeasurableT[MeasurementUnit] = [T] =>> Measurable[MeasurementUnit, T]

trait Measurable[MeasurementUnit, +T]:
  def placeInside(constrains: Bounds[MeasurementUnit]): Sized[MeasurementUnit, T]
end Measurable

given[MeasurementUnit]: FlatMap[MeasurableT[MeasurementUnit]] with
  override def flatMap[A, B](fa: Measurable[MeasurementUnit, A])
                            (f: A => Measurable[MeasurementUnit, B]): Measurable[MeasurementUnit, B] =
    (constrains: Bounds[MeasurementUnit]) => f(fa.placeInside(constrains).value).placeInside(constrains)

  override def map[A, B](fa: Measurable[MeasurementUnit, A])(f: A => B): Measurable[MeasurementUnit, B] = c =>
    val placed = fa.placeInside(c)
    Sized(f(placed.value), placed.width, placed.height)
  end map

  override def tailRecM[A, B](a: A)(f: A => Measurable[MeasurementUnit, Either[A, B]]): Measurable[MeasurementUnit, B] =
    c =>
      @tailrec
      def helper(a : A)(f: A => Measurable[MeasurementUnit, Either[A, B]]): Sized[MeasurementUnit, B] =
        f(a).placeInside(c) match
          case Sized(Left(a), _, _) => helper(a)(f)
          case Sized(Right(value), w, h) => Sized(value, w, h)
        end match
      end helper

      helper(a)(f)
  end tailRecM
end given