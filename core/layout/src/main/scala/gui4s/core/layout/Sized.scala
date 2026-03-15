package gui4s.core.layout

import cats.Comonad
import cats.Eq

import gui4s.core.geometry.Rect

type SizedC[Size] = Sized[Size, *]

final case class Sized[+Size, +T](value : T, size : Size):
  def mapValue[B](f : T => B) : Sized[Size, B] =
    copy(value = f(value))
  end mapValue
end Sized

object Sized:
  def apply[MeasurementUnit, T](value: T, x: MeasurementUnit, y: MeasurementUnit) =
    new Sized(value, Rect(x, y))
  end apply

  given sizedEq[MeasurementUnit : Eq, T : Eq] : Eq[Sized[MeasurementUnit, T]] =
    Eq.by(x => (x.value, x.size))
  end sizedEq

  given[MeasurementUnit]: Comonad[SizedC[MeasurementUnit]] with
    override def coflatMap[A, B](fa: Sized[MeasurementUnit, A])(f: Sized[MeasurementUnit, A] => B): Sized[MeasurementUnit, B] =
      fa.mapValue(_ => f(fa))
    end coflatMap

    override def extract[A](x: Sized[MeasurementUnit, A]): A =
      x.value
    end extract

    override def map[A, B](fa: Sized[MeasurementUnit, A])(f: A => B): Sized[MeasurementUnit, B] =
      fa.mapValue(f)
    end map
  end given
end Sized  