package me.katze.gui4s.layout

import bound.Bounds

import cats.*
import cats.syntax.all.*

type MeasurableT[F[+_], MeasurementUnit] = [Value] =>> Measurable[F, MeasurementUnit, Value]

type PlacePartial[+F[+_], -MeasurementUnit, +Value] = Bounds[MeasurementUnit] => F[Value]
type Measurable[+F[+_], MeasurementUnit, +Value] = PlacePartial[F, MeasurementUnit, Sized[MeasurementUnit, Value]]
  
given measurableIsFlatMap[F[+_] : Monad, MeasurementUnit]: FlatMap[MeasurableT[F, MeasurementUnit]] with
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
          case Sized(Left(a), _) => helper(a)(f)
          case Sized(Right(value), size) => Sized(value, size).pure[F]
      end helper

      helper(a)(f)
  end tailRecM
end measurableIsFlatMap