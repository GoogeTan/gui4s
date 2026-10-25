package catnip

import cats.Order

trait Sortable[Collection[_]]:
  def sort[A : Order](collection: Collection[A]): Collection[A]

  def sortBy[A, B : Order](collection: Collection[A], f : A => B): Collection[A] =
    given Order[A] = Order.by(f)
    sort(collection)
  end sortBy
end Sortable