package me.katze.gui4s.layout

import cats.Functor

final case class MaybeWeighted[+T](weight: Option[Int], value: T)


given Functor[MaybeWeighted] with 
  override def map[A, B](fab: MaybeWeighted[A])(f: A => B): MaybeWeighted[B] =
    MaybeWeighted(fab.weight, f(fab.value))
  end map
end given
