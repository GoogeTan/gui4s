package catnip
package syntax

import cats.Monad
import cats.data.{NonEmptyList, StateT}
import cats.{Applicative, Functor, Order, SemigroupK, Traverse}
import cats.syntax.all.*

object list:
  def fromList[C[_] : {Applicative as A, SemigroupK as S}, T](c : NonEmptyList[T]) : C[T] =
    c.reduceMapK(A.pure)
  end fromList
  
  /**
   * Производит обработку списка в указаном порядке элементов без нарушенения их порядка в результирующем списке.
   * @param list Список для обработки
   * @param f    Обработчик списка. Должен вернуть список той же длины, что и исходный
   */
  def orderedListProcessing[F[_] : Functor, C[_] : {Applicative as A, Traverse, SemigroupK}, A : Order, B](list: C[A])(f: C[A] => F[C[B]]): F[C[B]] =
    NonEmptyList.fromList(list.toList) match
      case Some(value) =>
        val indexed = value.zipWithIndex.sortBy((value, _) => value)
        val indexes = indexed.map(_._2)
        val values = fromList(indexed.map(_._1))
        f(values).map(res =>
          fromList(
            NonEmptyList.fromListUnsafe(
              res.toList
            ).zip(indexes).sortBy(_._2).map(_._1)
          )
        )
      case None => 
        f(list)
  end orderedListProcessing
  
  def traverseUntil[F[_] : Monad, G[_] : Traverse, A, B](original : G[A], main : A => F[(Boolean, B)], afterAll : A => F[B]) : F[G[B]] =
    original.traverse[StateT[F, Boolean, *], B](
      element =>
        StateT(
          shouldStop =>
            if shouldStop then
              afterAll(element).map(a => (true, a))
            else
              main(element)
        )
    ).runA(false)
  end traverseUntil
end list

  
