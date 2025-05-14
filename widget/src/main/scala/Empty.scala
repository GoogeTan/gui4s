package me.katze.gui4s.widget

import cats.Monoid

trait Empty[+T]:
  def empty : T
end Empty

def empty[T : Empty as E] = E.empty

given monoidHasEmpty[T](using m : Monoid[T]) : Empty[T] with
  override def empty: T = m.empty
end monoidHasEmpty