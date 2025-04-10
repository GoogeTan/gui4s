package me.katze.gui4s.layout

import bound.Bounds

import cats.*
import cats.syntax.all.*

type MeasurableT[F[+_], MeasurementUnit] = [T] =>> Measurable[F, MeasurementUnit, T]

type Measurable[+F[+_], MeasurementUnit, +T] = Bounds[MeasurementUnit] =>  F[Sized[MeasurementUnit, T]]
  
given[F[+_] : Monad, MeasurementUnit]: FlatMap[MeasurableT[F, MeasurementUnit]] with
  override def flatMap[A, B](fa: Measurable[F, MeasurementUnit, A])
                            (f: A => Measurable[F, MeasurementUnit, B]): Measurable[F, MeasurementUnit, B] =
    bounds => fa(bounds).flatMap(sized => f(sized.value)(bounds))
  end flatMap

  override def map[A, B](fa: Measurable[F, MeasurementUnit, A])(f: A => B): Measurable[F, MeasurementUnit, B] =
    bounds => fa(bounds).map(_.mapValue(f))
  end map

  override def tailRecM[A, B](a: A)(f: A => Measurable[F, MeasurementUnit, Either[A, B]]): Measurable[F, MeasurementUnit, B] =
    c =>
      def helper(a : A)(f: A => Measurable[F, MeasurementUnit, Either[A, B]]): F[Sized[MeasurementUnit, B]] =
        f(a)(c).flatMap:
          case Sized(Left(a), _, _) => helper(a)(f)
          case Sized(Right(value), w, h) => Sized(value, w, h).pure[F]
      end helper

      helper(a)(f)
  end tailRecM
end given