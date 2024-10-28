package me.katze.gui4s.widget
package library

import cats.Monoid

trait Empty[+T]:
  def empty : T
end Empty

given monoidHasEmpty[T](using m : Monoid[T]) : Empty[T] with
  override def empty: T = m.empty
end monoidHasEmpty