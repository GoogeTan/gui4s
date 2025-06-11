package me.katze.gui4s.layout

import bound.Bounds

import cats.*
import cats.syntax.all.*

type MeasurableT[F[+_], MeasurementUnit] = [Value] =>> Measurable[F, MeasurementUnit, Value]

type PlacePartial[+F[+_], -MeasurementUnit, +Value] = Bounds[MeasurementUnit] => F[Value]
type Measurable[+F[+_], MeasurementUnit, +Value] = PlacePartial[F, MeasurementUnit, Sized[MeasurementUnit, Value]]
  
given measurableIsFunctor[F[+_] : Monad, MeasurementUnit]: Functor[MeasurableT[F, MeasurementUnit]] with
  override def map[A, B](fa: Measurable[F, MeasurementUnit, A])(f: A => B): Measurable[F, MeasurementUnit, B] =
    bounds => fa(bounds).map(_.mapValue(f))
  end map
end measurableIsFunctor