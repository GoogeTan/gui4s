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

  given[Size]: Comonad[SizedC[Size]] with
    override def coflatMap[A, B](fa: Sized[Size, A])(f: Sized[Size, A] => B): Sized[Size, B] =
      fa.mapValue(_ => f(fa))
    end coflatMap

    override def extract[A](x: Sized[Size, A]): A =
      x.value
    end extract

    override def map[A, B](fa: Sized[Size, A])(f: A => B): Sized[Size, B] =
      fa.mapValue(f)
    end map
  end given
end Sized  