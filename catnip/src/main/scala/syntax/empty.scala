package catnip
package syntax

import cats.Monoid

object empty:
  given monoidHasEmpty[T](using m: Monoid[T]): Empty[T] with
    override def empty: T = m.empty
  end monoidHasEmpty

  def empty[T : Empty as E]: T = E.empty
end empty
