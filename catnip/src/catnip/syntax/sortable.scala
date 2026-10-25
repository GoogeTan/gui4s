package catnip.syntax

import catnip.Sortable
import cats.Id
import cats.Order

object sortable:
  given idIsSortable: Sortable[Id] =
    new Sortable[Id]:
      override def sort[A: Order](collection: Id[A]): Id[A] =
        collection
      end sort
    end new
  end idIsSortable
  
  given listIsSortable: Sortable[List] =
    new Sortable[List]:
      override def sort[A: Order](collection: List[A]): List[A] =
        collection.sorted(using (a, b) => Order[A].compare(a, b))
      end sort
  end listIsSortable
  
  extension[Collection[_], T](value : Collection[T])
    def sort(using sortable: Sortable[Collection], order: Order[T]): Collection[T] =
      sortable.sort(value)
    end sort
  end extension
end sortable
